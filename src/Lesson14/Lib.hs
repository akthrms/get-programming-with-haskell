module Lesson14.Lib where

-- Q14.1

data Number
  = One
  | Two
  | Three
  deriving (Enum)

instance Eq Number where
  (==) n1 n2 = (fromEnum n1) == (fromEnum n2)

instance Ord Number where
  compare n1 n2 = compare (fromEnum n1) (fromEnum n2)

-- Q14.2

data FiveSidedDie
  = Side1
  | Side2
  | Side3
  | Side4
  | Side5
  deriving (Enum, Eq, Ord, Show)

class (Eq a, Enum a) => Die a where
  roll :: Int -> a

instance Die FiveSidedDie where
  roll n = toEnum (n `mod` 5)
