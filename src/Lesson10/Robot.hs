module Lesson10.Robot where

robot :: (a, b, c) -> ((a, b, c) -> t) -> t
robot (name, attack, hp) = \message -> message (name, attack, hp)

killerRobot :: (([Char], Integer, Integer) -> t) -> t
killerRobot = robot ("Kill3r", 25, 200)

name :: (a, b, c) -> a
name (n, _, _) = n

attack :: (a, b, c) -> b
attack (_, a, _) = a

hp :: (a, b, c) -> c
hp (_, _, h) = h

getName :: (((a, b, c) -> a) -> t) -> t
getName aRobot = aRobot name

getAttack :: (((a, b, c) -> b) -> t) -> t
getAttack aRobot = aRobot attack

getHP :: (((a, b, c) -> c) -> t) -> t
getHP aRobot = aRobot hp

{-
*Lesson10.Robot> getAttack killerRobot
25
*Lesson10.Robot> getHP killerRobot
200
-}

setName :: (((a1, b, c) -> ((a2, b, c) -> t1) -> t1) -> t2) -> a2 -> t2
setName aRobot newName = aRobot (\(_, a, h) -> robot (newName, a, h))

setAttack :: (((a, b1, c) -> ((a, b2, c) -> t1) -> t1) -> t2) -> b2 -> t2
setAttack aRobot newAttack = aRobot (\(n, _, h) -> robot (n, newAttack, h))

setHP :: (((a, b, c1) -> ((a, b, c2) -> t1) -> t1) -> t2) -> c2 -> t2
setHP aRobot newHP = aRobot (\(n, a, _) -> robot (n, a, newHP))

printRobot :: (Show a1, Show a2) => ((([Char], a1, a2) -> [Char]) -> t) -> t
printRobot aRobot = aRobot (\(n, a, h) -> n ++ " attack:" ++ (show a) ++ " hp:" ++ (show h))

{-
*Lesson10.Robot> printRobot killerRobot
"Kill3r attack:25 hp:200"
-}

damage :: Num c => (((a, b, c) -> ((a, b, c) -> t1) -> t1) -> t2) -> c -> t2
damage aRobot attackDamage = aRobot (\(n, a, h) -> robot (n, a, h - attackDamage))

{-
*Lesson10.Robot> afterHit = damage killerRobot 90
*Lesson10.Robot> getHP afterHit
110
-}

fight :: (Ord a1, Num a1) => (((a2, b1, b1) -> b1) -> a1) -> (((a3, b2, a1) -> ((a3, b2, a1) -> t1) -> t1) -> t2) -> t2
fight aRobot defender =
  damage defender attack
  where
    attack = if getHP aRobot > 10 then getAttack aRobot else 0

gentleGiant :: (([Char], Integer, Integer) -> t) -> t
gentleGiant = robot ("Mr. Friendly", 10, 300)

{-
*Lesson10.Robot> gentleGiantRound1 = fight killerRobot gentleGiant
*Lesson10.Robot> killerRobotRound1 = fight gentleGiant killerRobot
*Lesson10.Robot> gentleGiantRound2 = fight killerRobotRound1 gentleGiantRound1
*Lesson10.Robot> killerRobotRound2 = fight gentleGiantRound1 killerRobotRound1
*Lesson10.Robot> gentleGiantRound3 = fight killerRobotRound2 gentleGiantRound2
*Lesson10.Robot> killerRobotRound3 = fight gentleGiantRound2 killerRobotRound2
*Lesson10.Robot> printRobot gentleGiantRound3
"Mr. Friendly attack:10 hp:225"
*Lesson10.Robot> printRobot killerRobotRound3
"Kill3r attack:25 hp:170"
-}
