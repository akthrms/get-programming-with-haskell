module Lesson17.Lib where

-- Q17.1

data Color
  = Red
  | Yellow
  | Blue
  | Green
  | Purple
  | Orange
  | Brown
  | Clear
  deriving (Show, Eq)

instance Semigroup Color where
  (<>) Clear color2 = color2
  (<>) color1 Clear = color1
  (<>) Red Blue = Purple
  (<>) Blue Red = Purple
  (<>) Yellow Blue = Green
  (<>) Blue Yellow = Green
  (<>) Yellow Red = Orange
  (<>) Red Yellow = Orange
  (<>) color1 color2
    | color1 == color2 = color1
    | all (`elem` [Red, Blue, Purple]) [color1, color2] = Purple
    | all (`elem` [Blue, Yellow, Green]) [color1, color2] = Green
    | all (`elem` [Red, Yellow, Orange]) [color1, color2] = Orange
    | otherwise = Brown

instance Monoid Color where
  mempty = Clear
  mappend color1 color2 = color1 <> color2

-- Q17.2

--

data Events = Events [String]

conbineEvents :: Events -> Events -> Events
conbineEvents (Events e1) (Events e2) = Events (cartConbine (\x y -> mconcat [x, "-", y]) e1 e2)

instance Semigroup Events where
  (<>) = conbineEvents

instance Monoid Events where
  mappend = (<>)
  mempty = Events []

--

data Probs = Probs [Double]

conbineProbs :: Probs -> Probs -> Probs
conbineProbs (Probs p1) (Probs p2) = Probs (cartConbine (*) p1 p2)

instance Semigroup Probs where
  (<>) = conbineProbs

instance Monoid Probs where
  mappend = (<>)
  mempty = Probs []

--

data Table = Table Events Probs

createTable :: Events -> Probs -> Table
createTable events (Probs p) =
  Table events (Probs normalized)
  where
    total = sum p
    normalized = map (\x -> x / total) p

showPair :: String -> Double -> String
showPair event prob = mconcat [event, "|", show prob, "\n"]

instance Show Table where
  show (Table (Events e) (Probs p)) = mconcat $ zipWith showPair e p

cartConbine :: (a1 -> a2 -> c) -> [a1] -> [a2] -> [c]
cartConbine f l1 l2 =
  zipWith f concatenatedL1 cycledL2
  where
    repeatedL1 = map (take (length l2) . repeat) l1
    concatenatedL1 = mconcat repeatedL1
    cycledL2 = cycle l2

instance Semigroup Table where
  (<>) table1 (Table (Events []) (Probs [])) = table1
  (<>) (Table (Events []) (Probs [])) table2 = table2
  (<>) (Table e1 p1) (Table e2 p2) = createTable (e1 <> e2) (p1 <> p2)

instance Monoid Table where
  mempty = Table (Events []) (Probs [])
  mappend = (<>)

--

coin :: Table
coin = createTable (Events ["heads", "tails"]) (Probs [0.5, 0.5])

spinner :: Table
spinner = createTable (Events ["red", "blue", "green"]) (Probs [0.1, 0.2, 0.7])

{-
*Lesson17.Lib> coin <> spinner
heads-red|5.0e-2
heads-blue|0.1
heads-green|0.35
tails-red|5.0e-2
tails-blue|0.1
tails-green|0.35

*Lesson17.Lib> mconcat [coin, coin, coin]
heads-heads-heads|0.125
heads-heads-tails|0.125
heads-tails-heads|0.125
heads-tails-tails|0.125
tails-heads-heads|0.125
tails-heads-tails|0.125
tails-tails-heads|0.125
tails-tails-tails|0.125
-}
