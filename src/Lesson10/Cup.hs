module Lesson10.Cup where

cup :: t1 -> (t1 -> t2) -> t2
cup flOz = \message -> message flOz

coffeeCup :: (Integer -> t2) -> t2
coffeeCup = cup 12

getOz :: ((p -> p) -> t) -> t
getOz aCup = aCup (\flOz -> flOz)

{-
*Lesson10.Cup> getOz coffeeCup
12
-}

drink :: (Ord t1, Num t1) => ((p -> p) -> t1) -> t1 -> (t1 -> t2) -> t2
drink aCup ozDrank =
  if ozDiff >= 0
    then cup (flOz - ozDrank)
    else cup 0
  where
    flOz = getOz aCup
    ozDiff = flOz - ozDrank

{-
*Lesson10.Cup> afterASip = drink coffeeCup 1
*Lesson10.Cup> getOz afterASip
11
*Lesson10.Cup> afterTwoSips = drink afterASip 1
*Lesson10.Cup> getOz afterTwoSips
10
*Lesson10.Cup> afterGulp = drink afterTwoSips 4
*Lesson10.Cup> getOz afterGulp
6

*Lesson10.Cup> afterBigGulp = drink coffeeCup 20
*Lesson10.Cup> getOz afterBigGulp
0
-}

isEmpty :: (Eq a, Num a) => ((p -> p) -> a) -> Bool
isEmpty aCup = getOz aCup == 0

afterManySips :: (Integer -> Integer) -> Integer
afterManySips = foldl drink coffeeCup [1, 1, 1, 1, 1]

{-
*Lesson10.Cup> getOz afterManySips
7
-}
