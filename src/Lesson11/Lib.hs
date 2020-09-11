module Lesson11.Lib where

-- Q11.1

myFilter :: (a -> Bool) -> [a] -> [a]
myFilter f [] = []
myFilter f (x : xs) = if f x then x : (myFilter f xs) else (myFilter f xs)

-- Q11.2

myTail :: [a] -> [a]
myTail [] = []
myTail (_ : xs) = xs

-- Q11.3

myFoldl :: (t -> a -> t) -> t -> [a] -> t
myFoldl f init [] = init
myFoldl f init (x : xs) = myFoldl f newInit xs
  where
    newInit = f init x
