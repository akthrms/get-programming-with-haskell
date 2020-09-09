module Lesson03.Lib where

-- Q3.1

inc :: Num a => a -> a
inc = \n -> n + 1

double :: Num a => a -> a
double = \n -> n * 2

square :: Num a => a -> a
square = \n -> n ^ 2

-- Q3.2

counter :: Num a => a -> a
counter x = (\x -> x + 1) ((\x -> x + 1) x)
