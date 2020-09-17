module Lesson18.Lib
  ( Organ,
  )
where

import qualified Data.Map as Map

-- Q18.1

data Box a = Box a deriving (Show)

wrap :: a -> Box a
wrap x = Box x

unwrap :: Box a -> a
unwrap (Box x) = x

data Triple a = Triple a a a deriving (Show)

first :: Triple a -> a
first (Triple x _ _) = x

second :: Triple a -> a
second (Triple _ x _) = x

third :: Triple a -> a
third (Triple _ _ x) = x

toList :: Triple a -> [a]
toList (Triple x y z) = [x, y, z]

transform :: (t -> a) -> Triple t -> Triple a
transform f (Triple x y z) = Triple (f x) (f y) (f z)

boxMap :: (t -> a) -> Box t -> Box a
boxMap f (Box x) = Box (f x)

tripleMap :: (t -> a) -> Triple t -> Triple a
tripleMap f (Triple x y z) = Triple (f x) (f y) (f z)

-- Q18.2

data Organ
  = Heart
  | Brain
  | Kidney
  | Spleen
  deriving (Show, Eq, Ord, Enum)

organs :: [Organ]
organs = [Heart, Heart, Brain, Spleen, Spleen, Kidney]

ids :: [Integer]
ids = [2, 7, 13, 14, 21, 24]

organPairs :: [(Integer, Organ)]
organPairs = zip ids organs

organCatalog :: Map.Map Integer Organ
organCatalog = Map.fromList organPairs

values :: [Organ]
values = map snd $ Map.toList organCatalog

allOrgans :: [Organ]
allOrgans = [Heart .. Spleen]

organCounts :: [Int]
organCounts = map (\organ -> (length . filter (== organ)) values) allOrgans

organInventory :: Map.Map Organ Int
organInventory = Map.fromList $ zip allOrgans organCounts
