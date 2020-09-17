module Lesson19.Lib where

import qualified Data.Map as Map
import Data.Maybe (isNothing)
import Lesson18.Lib (Organ)

-- Q19.1

getDrawerContents :: [Int] -> Map.Map Int Organ -> [Maybe Organ]
getDrawerContents ids catalog = map getContents ids
  where
    getContents = \id -> Map.lookup id catalog

emptyDrawers :: [Maybe Organ] -> Int
emptyDrawers maybeOrgans = (length . filter isNothing) maybeOrgans

-- Q19.2

maybeMap :: (t -> a) -> Maybe t -> Maybe a
maybeMap f (Just x) = Just $ f x
maybeMap f Nothing = Nothing
