module Lesson02.Lib where

-- Q2.1

-- Q2.2

inc :: Num a => a -> a
inc n = n + 1

double :: Num a => a -> a
double n = n * 2

square :: Num a => a -> a
square n = n ^ 2

-- Q2.3

q0203 :: Integral a => a -> a
q0203 n = if even n then n - 2 else 3 * n + 1
