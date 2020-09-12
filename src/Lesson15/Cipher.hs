module Lesson15.Cipher where

import Lesson15.Pad (applyOTP)
import Lesson15.Rot13 (charDecoder, charEncoder)

class Cipher a where
  encode :: a -> String -> String
  decode :: a -> String -> String

data Rot = Rot

instance Cipher Rot where
  encode Rot text = charEncoder text
  decode Rot text = charDecoder text

{-
*Lesson15.Cipher> encode Rot "Haskell"
"\557128\557153\557171\557163\557157\557164\557164"

*Lesson15.Cipher> decode Rot "\557128\557153\557171\557163\557157\557164\557164"
"Haskell"
-}

data OneTimePad = OTP String

instance Cipher OneTimePad where
  encode (OTP pad) text = applyOTP pad text
  decode (OTP pad) text = applyOTP pad text

myOTP :: OneTimePad
myOTP = OTP (cycle [minBound .. maxBound])

{-
*Lesson15.Cipher> encode myOTP "Learn Haskell"
"Ldcqj%Nf{bog`"

*Lesson15.Cipher> decode myOTP "Ldcqj%Nf{bog`"
"Learn Haskell"
-}

prng :: Integral a => a -> a -> a -> a -> a
prng a b maxNumber seed = (a * seed + b) `mod` maxNumber

examplePRNG :: Integer -> Integer
examplePRNG = prng 1337 7 100

{-
*Lesson15.Cipher> examplePRNG 12345
72

*Lesson15.Cipher> examplePRNG 72
71

*Lesson15.Cipher> examplePRNG 71
34

*Lesson15.Cipher> examplePRNG 34
65
-}
