module Lesson15.Pad
  ( applyOTP,
  )
where

import Lesson15.Bits (Bits, bitsToChar, charToBits)
import Lesson15.Xor (xor)

myPad :: [Char]
myPad = "Shhhhhh"

myText :: [Char]
myText = "Haskell"

applyOTP' :: [Char] -> [Char] -> [Bits]
applyOTP' pad text =
  map (\pair -> (fst pair) `xor` (snd pair)) $ zip padBits textBits
  where
    padBits = map charToBits pad
    textBits = map charToBits text

applyOTP :: [Char] -> [Char] -> [Char]
applyOTP pad text = map bitsToChar $ applyOTP' pad text

{-
*Lesson15.Pad> applyOTP myPad myText
"\ESC\t\ESC\ETX\r\EOT\EOT"
-}

encoderDecoder :: [Char] -> [Char]
encoderDecoder = applyOTP myPad

{-
*Lesson15.Pad> encoderDecoder "book"
"1\a\a\ETX"

*Lesson15.Pad> encoderDecoder "1\a\a\ETX"
"book"
-}
