module Lesson15.Bits
  ( Bits,
    bitsToChar,
    charToBits,
  )
where

type Bits = [Bool]

intToBits' :: Int -> Bits
intToBits' 0 = [False]
intToBits' 1 = [True]
intToBits' n =
  if remainder == 0
    then False : intToBits' next
    else True : intToBits' next
  where
    remainder = n `mod` 2
    next = n `div` 2

{-
*Lesson15.Bits> intToBits' 2
[False,True]

*Lesson15.Bits> intToBits' 8
[False,False,False,True]
-}

maxBits :: Int
maxBits = length $ intToBits' (maxBound :: Int)

intToBits :: Int -> Bits
intToBits n =
  falses ++ reversed
  where
    reversed = reverse $ intToBits' n
    missing = maxBits - length reversed
    falses = take missing $ cycle [False]

{-
*Lesson15.Bits> intToBits 2
[False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,True,False]

*Lesson15.Bits> intToBits (maxBound :: Int)
[True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True]
-}

charToBits :: Char -> Bits
charToBits char = intToBits $ fromEnum char

bitsToInt :: Bits -> Int
bitsToInt bits =
  sum $ map (\x -> 2 ^ (snd x)) trueLocations
  where
    size = length bits
    indices = [size - 1, size - 2 .. 0]
    trueLocations = filter (\x -> fst x == True) $ zip bits indices

{-
*Lesson15.Bits> bitsToInt $ intToBits 32
32

*Lesson15.Bits> bitsToInt $ intToBits (maxBound :: Int)
9223372036854775807
-}

bitsToChar :: Bits -> Char
bitsToChar bits = toEnum $ bitsToInt bits

{-
*Lesson15.Bits> bitsToChar $ charToBits 'a'
'a'

*Lesson15.Bits> bitsToChar $ charToBits (maxBound :: Char)
'\1114111'

*Lesson15.Bits> bitsToChar $ charToBits (minBound :: Char)
'\NUL'
-}
