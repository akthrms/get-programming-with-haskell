module Lesson13.Lib where

-- Q13.1

-- Q13.2

-- Q13.3

cycleSucc :: (Eq p, Bounded p, Enum p) => p -> p
cycleSucc n = if n == maxBound then minBound else succ n
