module Lesson09.Lib where

import Data.Char (toLower)

-- Q9.1

myelem :: Eq a => a -> [a] -> Bool
myelem a xs = length (filter (\x -> a == x) xs) /= 0

-- Q9.2

isPalindrome :: [Char] -> Bool
isPalindrome str = str' == reverse str'
  where
    str' = map toLower $ filter (\c -> c /= ' ') str

-- Q9.3

harmonic :: (Fractional a, Enum a) => Int -> a
harmonic n = sum $ take n lst
  where
    lst = map (\pair -> (fst pair) / (snd pair)) $ zip (cycle [1.0]) [1.0, 2.0 ..]
