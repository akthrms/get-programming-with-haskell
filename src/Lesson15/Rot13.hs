module Lesson15.Rot13
  ( charDecoder,
    charEncoder,
  )
where

rotEncoder :: (Bounded a, Enum a) => Int -> a -> a
rotEncoder size enum =
  toEnum rotation
  where
    half = size `div` 2
    offset = fromEnum enum + half
    rotation = offset `mod` size

rotDecoder :: (Bounded a, Enum a) => Int -> a -> a
rotDecoder size enum =
  toEnum rotation
  where
    half = size `div` 2
    offset = if even size then fromEnum enum + half else 1 + fromEnum enum + half
    rotation = offset `mod` size

data FourLetterAlphabet
  = L1
  | L2
  | L3
  | L4
  deriving (Show, Enum, Bounded)

fourLetterMessage :: [FourLetterAlphabet]
fourLetterMessage = [L1, L3, L4, L1, L1, L2]

fourLetterEncoder :: [FourLetterAlphabet] -> [FourLetterAlphabet]
fourLetterEncoder vals = map (rotEncoder $ 1 + fromEnum (maxBound :: FourLetterAlphabet)) vals

fourLetterDecoder :: [FourLetterAlphabet] -> [FourLetterAlphabet]
fourLetterDecoder vals = map (rotDecoder $ 1 + fromEnum (maxBound :: FourLetterAlphabet)) vals

{-
*Lesson15.Rot13> fourLetterMessage
[L1,L3,L4,L1,L1,L2]

*Lesson15.Rot13> fourLetterEncoder fourLetterMessage
[L3,L1,L2,L3,L3,L4]

*Lesson15.Rot13> fourLetterDecoder $ fourLetterEncoder fourLetterMessage
[L1,L3,L4,L1,L1,L2]
-}

data ThreeLetterAlphabet
  = Alpha
  | Beta
  | Kappa
  deriving (Show, Enum, Bounded)

threeLetterMessage :: [ThreeLetterAlphabet]
threeLetterMessage = [Alpha, Alpha, Beta, Alpha, Kappa]

threeLetterEncoder :: [ThreeLetterAlphabet] -> [ThreeLetterAlphabet]
threeLetterEncoder vals = map (rotEncoder $ 1 + fromEnum (maxBound :: ThreeLetterAlphabet)) vals

threeLetterDecoder :: [ThreeLetterAlphabet] -> [ThreeLetterAlphabet]
threeLetterDecoder vals = map (rotDecoder $ 1 + fromEnum (maxBound :: ThreeLetterAlphabet)) vals

{-
*Lesson15.Rot13> threeLetterMessage
[Alpha,Alpha,Beta,Alpha,Kappa]

*Lesson15.Rot13> threeLetterEncoder threeLetterMessage
[Beta,Beta,Kappa,Beta,Alpha]

*Lesson15.Rot13> threeLetterDecoder $ threeLetterEncoder threeLetterMessage
[Alpha,Alpha,Beta,Alpha,Kappa]
-}

charEncoder :: [Char] -> [Char]
charEncoder vals = map (rotEncoder $ 1 + fromEnum (maxBound :: Char)) vals

charDecoder :: [Char] -> [Char]
charDecoder vals = map (rotDecoder $ 1 + fromEnum (maxBound :: Char)) vals

{-
*Lesson15.Rot13> charEncoder "Jean-Paul likes Simone"
"\557130\557157\557153\557166\557101\557136\557153\557173\557164\557088\557164\557161\557163\557157\557171\557088\557139\557161\557165\557167\557166\557157"

*Lesson15.Rot13> charDecoder $ charEncoder "Jean-Paul likes Simone"
"Jean-Paul likes Simone"
-}
