module Lesson15.Xor where

xorBool :: Bool -> Bool -> Bool
xorBool value1 value2 = (value1 || value2) && (not (value1 && value2))

xorPair :: (Bool, Bool) -> Bool
xorPair (value1, value2) = xorBool value1 value2

xor :: [Bool] -> [Bool] -> [Bool]
xor list1 list2 = map xorPair $ zip list1 list2
