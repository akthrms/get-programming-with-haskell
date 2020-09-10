module Lesson07.Lib where

-- Q7.1

myTail :: [a] -> [a]
myTail (_ : xs) = xs
myTail [] = []

-- Q7.2

myGCD :: Integral t => t -> t -> t
myGCD a 0 = a
myGCD a b = myGCD b (a `mod` b)
