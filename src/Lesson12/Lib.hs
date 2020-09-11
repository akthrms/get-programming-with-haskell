module Lesson12.Lib where

-- Q12.1

type FirstName = String

type LastName = String

type MiddleName = String

data Name
  = Name FirstName LastName
  | NameWithMiddle FirstName MiddleName LastName

data Sex
  = Male
  | Female

data RhType
  = Pos
  | Neg

data ABOType
  = A
  | B
  | AB
  | O

data BloodType = BloodType ABOType RhType

data Patient = Patient
  { name :: Name,
    sex :: Sex,
    age :: Int,
    height :: Int,
    weight :: Int,
    bloodType :: BloodType
  }

canDonateTo :: BloodType -> BloodType -> Bool
canDonateTo (BloodType O _) _ = True
canDonateTo _ (BloodType AB _) = True
canDonateTo (BloodType A _) (BloodType A _) = True
canDonateTo (BloodType B _) (BloodType B _) = True
canDonateTo _ _ = False

donateFor :: Patient -> Patient -> Bool
donateFor p1 p2 = canDonateTo (bloodType p1) (bloodType p2)

-- Q12.2

showName :: Name -> [Char]
showName (Name fn ln) = fn ++ ", " ++ ln
showName (NameWithMiddle fn mn ln) = fn ++ ", " ++ mn ++ ", " ++ ln

showSex :: Sex -> [Char]
showSex Male = "Male"
showSex Female = "Female"

showRhType :: RhType -> [Char]
showRhType Pos = "+"
showRhType Neg = "-"

showABOType :: ABOType -> [Char]
showABOType A = "A"
showABOType B = "B"
showABOType AB = "AB"
showABOType O = "O"

showBloodType :: BloodType -> [Char]
showBloodType (BloodType abo rh) = showABOType abo ++ showRhType rh

patientSummary :: Patient -> [Char]
patientSummary patient =
  "***************"
    ++ "\nPatient Name: "
    ++ showName (name patient)
    ++ "\nSex: "
    ++ showSex (sex patient)
    ++ "\nAge: "
    ++ show (age patient)
    ++ "\nHeight: "
    ++ show (height patient)
    ++ "\nWeight: "
    ++ show (weight patient)
    ++ "\nBlood Type: "
    ++ showBloodType (bloodType patient)
