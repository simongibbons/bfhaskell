{-# LANGUAGE GeneralizedNewtypeDeriving, FlexibleInstances #-}

import Zipper
import Data.Word
import System.Environment
import Data.Char (chr)
import System.IO

data Op = Jump    Int
        | AddData Int
        | Output
        | Input
        | Loop [Op]
  deriving (Show)


data BFProgram = BFProgram { program   :: [Op]
                           , cells     :: Zipper Word16
                           }

instance Show BFProgram where
    show = (show.cells)

initialise :: [Op] -> BFProgram
initialise s = BFProgram s (fromList $ take 100000 $ repeat 0)

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
step (BFProgram (Input:os) s )        = do
                                         nv <- hGetChar stdin
                                         return (BFProgram os (setCElem ((fromIntegral.fromEnum) nv) s ))
step p@(BFProgram ((Loop np):os) s )  = runLoop p

runLoop :: BFProgram -> IO (BFProgram)
runLoop (BFProgram ((Loop np):os) s ) | (getCElem s) == 0 = return (BFProgram os s)
                                      | otherwise         = do
                                                             (BFProgram _ ns ) <- run (BFProgram np s)
                                                             if ( (getCElem ns) == 0 ) 
                                                             then return (BFProgram os ns )
                                                             else runLoop (BFProgram ((Loop np):os) ns )

optimise :: [Op] -> [Op]
optimise []   = []
optimise ((Jump n):(Jump m):xs) = (Jump (n+m)):(optimise xs)
optimise ((AddData n):(AddData m):xs) = (AddData (n+m)):(optimise xs)
optimise ((Loop p):xs) = (Loop (optimise p)):(optimise xs)
optimise (x:xs) = x:(optimise xs)

main :: IO ()
main = do
          args <- getArgs
          program <- readFile (head args)
          (run.initialise.optimise.parseProg) program
          return ()
