module Zipper where

data Zipper a = Zipper [a] [a]
    deriving (Show)

(+++) :: Zipper a -> Zipper a -> Zipper a
(Zipper l1 r1) +++ (Zipper l2 r2) = Zipper l1 (r1 ++ (reverse l2) ++ r2)

empty :: Zipper a
empty = Zipper [] []

incCur :: Zipper a -> Zipper a
incCur (Zipper l [] )    = Zipper l []
incCur (Zipper l (r:[]) )    = Zipper l (r:[])
incCur (Zipper l (r:rs)) = Zipper (r:l) rs

decCur :: Zipper a -> Zipper a
decCur (Zipper [] r ) = Zipper [] r
decCur (Zipper (l:ls) r) = Zipper ls (l:r)

jump :: Int -> Zipper a -> Zipper a
jump n x | n == 0    = x
         | n > 0     = jump (n-1) ( incCur x ) --Faster this rather than with take/drop
         | otherwise = jump (n+1) ( decCur x ) --due to slow list concatination

getCElem :: Zipper a -> a
getCElem (Zipper _ (r:rs) ) = r

setCElem :: a -> Zipper a -> Zipper a
setCElem _ (Zipper l [] )     = Zipper l []
setCElem x (Zipper l (r:rs) ) = (Zipper l (x:rs))

push :: a -> Zipper a -> Zipper a
push x (Zipper l r) = (Zipper l (x:r))

delCElem :: Zipper a -> Zipper a
delCElem (Zipper l (r:rs) ) = Zipper l rs

fromList :: [a] -> Zipper a
fromList x = Zipper [] x

toList :: Zipper a -> [a]
toList (Zipper l r) = (reverse l) ++ r
