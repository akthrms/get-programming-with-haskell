module Lesson06.Lib where

-- Q6.1

repaet :: a -> [a]
repaet x = cycle [x]

-- Q6.2

subseq :: Int -> Int -> [a] -> [a]
subseq start end xs =
  take diff (drop start xs)
  where
    diff = end - start

-- Q6.3

inFirstHalf :: Eq a => a -> [a] -> Bool
inFirstHalf x xs =
  x `elem` take midpoint xs
  where
    len = length xs
    midpoint = len `div` 2
