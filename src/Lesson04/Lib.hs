module Lesson04.Lib where

-- Q4.1

compareLastNames :: (Ord a1, Ord a2) => (a1, a2) -> (a1, a2) -> Ordering
compareLastNames name1 name2 =
  if result == EQ
    then compare (fst name1) (fst name2)
    else result
  where
    result = compare (snd name1) (snd name2)

-- Q4.2

nameText :: ([Char], [Char]) -> [Char]
nameText name = (fst name) ++ " " ++ (snd name)

dcOffice :: ([Char], [Char]) -> [Char]
dcOffice name = (nameText name) ++ ", Esq. PO Box 1337 - Washington DC, 20001"

getLocationFunction :: [Char] -> ([Char], [Char]) -> [Char]
getLocationFunction location =
  case location of
    "dc" -> dcOffice
    _ -> nameText
