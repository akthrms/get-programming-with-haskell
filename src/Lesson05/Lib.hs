module Lesson05.Lib where

-- Q5.1

ifEven :: Integral p => (p -> p) -> p -> p
ifEven f x = if even x then f x else x

ifEvenInc :: Integer -> Integer
ifEvenInc = ifEven (\x -> x + 1)

ifEvenDouble :: Integer -> Integer
ifEvenDouble = ifEven (\x -> x * 2)

ifEvenSquare :: Integer -> Integer
ifEvenSquare = ifEven (\x -> x ^ 2)

-- Q5.2

binaryPartialApplication :: (t1 -> t2 -> t3) -> t1 -> t2 -> t3
binaryPartialApplication f x = (\y -> f x y)
