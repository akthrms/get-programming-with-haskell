module Lesson20.TimeSeries where

import qualified Data.Map as Map
import Data.Maybe (fromJust, isJust, isNothing)

file1 :: [(Int, Double)]
file1 =
  [ (1, 200.1),
    (2, 199.5),
    (3, 199.4),
    (4, 198.9),
    (5, 199.0),
    (6, 200.2),
    (9, 200.3),
    (10, 201.2),
    (12, 202.9)
  ]

file2 :: [(Int, Double)]
file2 =
  [ (11, 201.6),
    (12, 201.5),
    (13, 201.5),
    (14, 203.5),
    (15, 204.9),
    (16, 207.1),
    (18, 210.5),
    (20, 208.8)
  ]

file3 :: [(Int, Double)]
file3 =
  [ (10, 201.2),
    (11, 201.6),
    (12, 201.5),
    (13, 201.5),
    (14, 203.5),
    (17, 210.5),
    (24, 215.1),
    (25, 218.7)
  ]

file4 :: [(Int, Double)]
file4 =
  [ (26, 219.8),
    (27, 220.5),
    (28, 223.8),
    (29, 222.8),
    (30, 223.8),
    (31, 221.7),
    (32, 222.3),
    (33, 220.8),
    (34, 219.4),
    (35, 220.1),
    (36, 220.6)
  ]

data TS a = TS [Int] [Maybe a]

createTS :: [Int] -> [a] -> TS a
createTS times values =
  let completeTimes = [minimum times .. maximum times]
      timeValueMap = Map.fromList $ zip times values
      extendedValues = map (`Map.lookup` timeValueMap) completeTimes
   in TS completeTimes extendedValues

fileToTS :: [(Int, a)] -> TS a
fileToTS tvPairs =
  let (times, values) = unzip tvPairs in createTS times values

showTVPair :: Show a => Int -> Maybe a -> String
showTVPair time (Just value) = mconcat [show time, "|", show value, "\n"]
showTVPair time Nothing = mconcat [show time, "|NA\n"]

instance Show a => Show (TS a) where
  show (TS times values) = mconcat $ zipWith showTVPair times values

{-
*Lesson20.TimeSeries> fileToTS file1
1|200.1
2|199.5
3|199.4
4|198.9
5|199.0
6|200.2
7|NA
8|NA
9|200.3
10|201.2
11|NA
12|202.9
-}

ts1 :: TS Double
ts1 = fileToTS file1

ts2 :: TS Double
ts2 = fileToTS file2

ts3 :: TS Double
ts3 = fileToTS file3

ts4 :: TS Double
ts4 = fileToTS file4

insertMaybePair :: Ord k => Map.Map k a -> (k, Maybe a) -> Map.Map k a
insertMaybePair myMap (_, Nothing) = myMap
insertMaybePair myMap (key, Just value) = Map.insert key value myMap

combineTS :: TS a -> TS a -> TS a
combineTS (TS [] []) ts2 = ts2
combineTS ts1 (TS [] []) = ts1
combineTS (TS t1 v1) (TS t2 v2) =
  let bothTimes = mconcat [t1, t2]
      completeTimes = [minimum bothTimes .. maximum bothTimes]
      timeValueMap = foldl insertMaybePair Map.empty $ zip t1 v1
      updatedMap = foldl insertMaybePair timeValueMap $ zip t2 v2
      combinedValues = map (`Map.lookup` updatedMap) completeTimes
   in TS completeTimes combinedValues

instance Semigroup (TS a) where
  (<>) = combineTS

{-
*Lesson20.TimeSeries> ts1 <> ts2
1|200.1
2|199.5
3|199.4
4|198.9
5|199.0
6|200.2
7|NA
8|NA
9|200.3
10|201.2
11|201.6
12|201.5
13|201.5
14|203.5
15|204.9
16|207.1
17|NA
18|210.5
19|NA
20|208.8
-}

instance Monoid (TS a) where
  mempty = TS [] []
  mappend = (<>)

{-
*Lesson20.TimeSeries> mconcat [ts1, ts2]
1|200.1
2|199.5
3|199.4
4|198.9
5|199.0
6|200.2
7|NA
8|NA
9|200.3
10|201.2
11|201.6
12|201.5
13|201.5
14|203.5
15|204.9
16|207.1
17|NA
18|210.5
19|NA
20|208.8
-}

tsAll :: TS Double
tsAll = mconcat [ts1, ts2, ts3, ts4]

mean :: Real a => [a] -> Double
mean xs =
  let total = (realToFrac . sum) xs
      count = (realToFrac . length) xs
   in total / count

meanTS :: Real a => TS a -> Maybe Double
meanTS (TS _ []) = Nothing
meanTS (TS times values) =
  let avg = mean $ map fromJust $ filter isJust values
   in if all isNothing values then Nothing else Just avg

{-
*Lesson20.TimeSeries> meanTS tsAll
Just 210.5966666666667
-}

type CompareFunc a = (a -> a -> a)

type TSCompareFunc a = ((Int, Maybe a) -> (Int, Maybe a) -> (Int, Maybe a))

makeTSCompare :: Eq a => CompareFunc a -> TSCompareFunc a
makeTSCompare func =
  let newFunc (i1, Nothing) (i2, Nothing) = (i1, Nothing)
      newFunc (_, Nothing) (i2, val2) = (i2, val2)
      newFunc (i1, val1) (_, Nothing) = (i1, val1)
      newFunc (i1, Just val1) (i2, Just val2) =
        if (func val1 val2) == val1 then (i1, Just val1) else (i2, Just val2)
   in newFunc

{-
*Lesson20.TimeSeries> makeTSCompare max (3, Just 200) (4, Just 10)
(3,Just 200)
-}

compareTS :: Eq a => (a -> a -> a) -> TS a -> Maybe (Int, Maybe a)
compareTS func (TS [] []) = Nothing
compareTS func (TS times values) =
  let best = foldl (makeTSCompare func) (0, Nothing) $ zip times values
   in if all isNothing values then Nothing else Just best

minTS :: Ord a => TS a -> Maybe (Int, Maybe a)
minTS = compareTS min

maxTS :: Ord a => TS a -> Maybe (Int, Maybe a)
maxTS = compareTS max

diffPair :: Num a => Maybe a -> Maybe a -> Maybe a
diffPair Nothing _ = Nothing
diffPair _ Nothing = Nothing
diffPair (Just x) (Just y) = Just (x - y)

diffTS :: Num a => TS a -> TS a
diffTS (TS [] []) = TS [] []
diffTS (TS times values) =
  let shiftValues = tail values
      diffValues = zipWith diffPair shiftValues values
   in TS times (Nothing : diffValues)

{-
*Lesson20.TimeSeries> meanTS $ diffTS tsAll
Just 0.6076923076923071
-}

meanMaybe :: Real a => [Maybe a] -> Maybe Double
meanMaybe vals =
  let avg = mean $ map fromJust vals
   in if any isNothing vals then Nothing else Just avg

movingAvg :: Real a => [Maybe a] -> Int -> [Maybe Double]
movingAvg [] n = []
movingAvg vals n =
  let nextVals = take n vals
      restVals = tail vals
   in if length nextVals == n then (meanMaybe nextVals) : (movingAvg restVals n) else []

maTS :: Real a => TS a -> Int -> TS Double
maTS (TS [] []) n = TS [] []
maTS (TS times values) n =
  let ma = movingAvg values n
      nothings = take (n `div` 2) $ repeat Nothing
      smoothedValues = mconcat [nothings, ma, nothings]
   in TS times smoothedValues
