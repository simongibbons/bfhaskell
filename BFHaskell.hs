import Zipper
import Data.Word
import System.Environment
import Data.Char (chr)
import System.IO

data Op = Jump    Int
        | AddData Int
        | Seek    Int     --Skipping n cells at a time find next 0
        | Output
        | Input
        | SetVal  Int
        | FarAdd  Int Int
        | Loop [Op]
  deriving (Show,Eq)


data BFProgram = BFProgram { program   :: [Op]
                           , cells     :: Zipper Word16
                           }

instance Show BFProgram where
    show = (show.cells)

initialise :: [Op] -> BFProgram
initialise s = BFProgram s (fromList $ take 300000 $ repeat 0)

findLoopEnd :: Int -> String -> String -> (String, String)
findLoopEnd n (']':xs) y | n == 0    = (reverse y, xs)
                         | otherwise =  findLoopEnd (n-1) xs (']':y)
findLoopEnd n ('[':xs) y = findLoopEnd (n+1) xs ('[':y)
findLoopEnd n (x:xs) y   = findLoopEnd n xs (x:y)

parseProg :: String -> [Op]
parseProg "" = []
parseProg (x:xs) | x == '>'  = (Jump   1)              : parseProg xs
                 | x == '<'  = (Jump  (-1))            : parseProg xs
                 | x == '+'  = (AddData 1)             : parseProg xs
                 | x == '-'  = (AddData (-1))          : parseProg xs
                 | x == '.'  = Output                  : parseProg xs
                 | x == ','  = Input                   : parseProg xs
                 | x == '['  = Loop (parseProg loop)   : parseProg rest
                 | otherwise = parseProg xs
    where (loop, rest) = findLoopEnd 0 xs ""

isEnd :: BFProgram -> Bool
isEnd (BFProgram [] _ ) = True
isEnd _                 = False

run :: BFProgram -> IO (BFProgram)
run p = if isEnd p then return p
         else do p' <- step p
                 run p'

step :: BFProgram -> IO (BFProgram)
step (BFProgram ((Jump n):os)  s )   = return (BFProgram os (jump n s) )
step (BFProgram ((AddData n):os) s ) = return (BFProgram os (setCElem ((getCElem s) + (fromIntegral n)) s ) )
step (BFProgram ((Output):os)    s ) = do
                                        hPutChar stdout $ (chr.fromEnum.getCElem) s
                                        hFlush  stdout
                                        return (BFProgram os s)
step (BFProgram ((SetVal n):os) s )  = return (BFProgram os (setCElem (fromIntegral n) s) )
step (BFProgram (Input:os) s )        = do
                                         nv <- hGetChar stdin
                                         return (BFProgram os (setCElem ((fromIntegral.fromEnum) nv) s ))
step (BFProgram ((FarAdd n m):os) s) = return (BFProgram os (((jump (-n)).(setCElem toAdd).(jump n)) s) )
    where toAdd = (fromIntegral m) * (getCElem s) + ((getCElem.(jump n)) s )

step (BFProgram ((Seek n):os) s) = return (BFProgram os (search n s))
  where
        search m s | x == 0    = s
                   | otherwise = search m ns
          where ns = jump m s
                x  = getCElem s

step p@(BFProgram ((Loop np):os) s )  = runLoop p

runLoop :: BFProgram -> IO (BFProgram)
runLoop (BFProgram ((Loop np):os) s ) | (getCElem s) == 0 = return (BFProgram os s)
                                      | otherwise         = do
                                                             (BFProgram _ ns ) <- run (BFProgram np s)
                                                             if ( (getCElem ns) == 0 ) 
                                                             then return (BFProgram os ns )
                                                             else runLoop (BFProgram ((Loop np):os) ns )


optimise :: [Op] -> [Op]
optimise p | p' == p = p
           | otherwise = optimise p'
 where p' = optimise' p
       optimise' []                           = []
       optimise' ((Jump 0):xs)                = optimise' xs
       optimise' ((AddData 0):xs)             = optimise' xs
       optimise' ((Loop []):xs)               = optimise' xs
       optimise' ((Jump n):(Jump m):xs)       = optimise' ((Jump (n+m)):xs)
       optimise' ((AddData n):(AddData m):xs) = optimise' ((AddData (n+m)):xs)
       optimise' ((Loop [AddData (-1)]):xs)   = (SetVal 0):(optimise' xs)
       optimise' ((Loop p):xs)                = (loopOptimise (optimise' p)) ++
                                                (optimise' xs)
       optimise' (x:xs)                       = x:(optimise' xs)

loopOptimise :: [Op] -> [Op]
loopOptimise p@[AddData (-1), Jump n1, AddData m, Jump (n2)] | n1==(-n2) = [FarAdd n1 m, SetVal 0]
                                                             | otherwise = [Loop p]
loopOptimise p@[Jump n1, AddData m, Jump (n2), AddData (-1)] | n1==(-n2) = [FarAdd n1 m, SetVal 0]
                                                             | otherwise = [Loop p]
loopOptimise p@[Jump n] = [Seek n]
loopOptimise p = [Loop p]

main :: IO ()
main = do
          args <- getArgs
          program <- readFile (head args)
          (run.initialise.optimise.parseProg) program
          return ()
