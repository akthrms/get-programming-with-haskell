module Lesson16.Lib where

-- Q16.1

type FirstName = String

type LastName = String

type MiddleName = String

data Name
  = Name FirstName LastName
  | NameWithMiddle FirstName MiddleName LastName
  | TwoInitialsWithLast Char Char LastName
  | FirstWithTwoInitials FirstName Char Char

data Creator
  = AuthorCreator Author
  | ArtistCreator Artist

data Author = Author Name

data Artist
  = Person Name
  | Band String

data Book = Book
  { bookAuthor :: Creator,
    bookIsbn :: String,
    bookTitle :: String,
    bookYear :: Int,
    bookPrice :: Double
  }

data VinylRecord = VinylRecord
  { vinylRecordArtist :: Creator,
    vinylRecordTitle :: String,
    vinylRecordYear :: Int,
    vinylRecordPrice :: Double
  }

data CollectiveToy = CollectiveToy
  { collectiveToyName :: String,
    collectiveToyDescription :: String,
    collectiveToyPrice :: Double
  }

data Pamphlet = Pamphlet
  { pamphletTitle :: String,
    pamphletDescription :: String,
    pamphletContact :: String
  }

data StoreItem
  = BookItem Book
  | VinylRecordItem VinylRecord
  | CollectiveToyItem CollectiveToy
  | PamphletItem Pamphlet

price :: StoreItem -> Double
price (BookItem book) = bookPrice book
price (VinylRecordItem vinylRecord) = vinylRecordPrice vinylRecord
price (CollectiveToyItem collectiveToy) = collectiveToyPrice collectiveToy
price (PamphletItem _) = 0.0

-- Q16.2

type Radius = Double

type Height = Double

type Width = Double

data Shape
  = Circle Radius
  | Square Height
  | Rectangle Height Width
  deriving (Show)

perimeter :: Shape -> Double
perimeter (Circle radius) = 2 * pi * radius
perimeter (Square height) = 4 * height
perimeter (Rectangle height width) = 2 * height + 2 * width

area :: Shape -> Double
area (Circle radius) = pi * radius ^ 2
area (Square height) = height ^ 2
area (Rectangle height width) = height * width
