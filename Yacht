{-
Scores in Yacht
Category	Score	Description	Example
Ones	1 × number of ones	Any combination	1 1 1 4 5 scores 3
Twos	2 × number of twos	Any combination	2 2 3 4 5 scores 4
Threes	3 × number of threes	Any combination	3 3 3 3 3 scores 15
Fours	4 × number of fours	Any combination	1 2 3 3 5 scores 0
Fives	5 × number of fives	Any combination	5 1 5 2 5 scores 15
Sixes	6 × number of sixes	Any combination	2 3 4 5 6 scores 6
Full House	Total of the dice	Three of one number and two of another	3 3 3 5 5 scores 19
Four of a Kind	Total of the four dice	At least four dice showing the same face	4 4 4 4 6 scores 16
Little Straight	30 points	1-2-3-4-5	1 2 3 4 5 scores 30
Big Straight	30 points	2-3-4-5-6	2 3 4 5 6 scores 30
Choice	Sum of the dice	Any combination	2 3 3 4 6 scores 18
Yacht	50 points	All five dice showing the same face	4 4 4 4 4 scores 50
If the dice do not satisfy the requirements of a category, the score is zero. If, for example, Four Of A Kind is entered in the Yacht category, zero points are scored. A Yacht scores zero if entered in the Full House category.
-}

module Yacht (yacht, Category(..)) where

import Data.List 

data Category = Ones
              | Twos
              | Threes
              | Fours
              | Fives
              | Sixes
              | FullHouse
              | FourOfAKind
              | LittleStraight
              | BigStraight
              | Choice
              | Yacht

d1 = [1,2,2,5,6] :: [Int]

d7 = [4,3,2,5,6] :: [Int]
d2 = [2,1,3,4,5] :: [Int]
d3 = [1,3,4,5,6] :: [Int]
d4 = [1,2,3,4,6] :: [Int]
d5 = [1,1,2,3,4] :: [Int]
d6 = [3,2,2,1,4] :: [Int]
d8 = [5,4,3,2,1] :: [Int]
fh = [2,2,3,2,3] :: [Int]
st = [2,3,4,5,6] :: [Int]
fourk = [1,5,5,5,5] :: [Int]
ya = [4,4,4,4,4] :: [Int]


yacht :: Category -> [Int] -> Int
yacht category dice = case category of
    Ones -> simpleNum dice 1
    Twos -> simpleNum dice 2
    Threes -> simpleNum dice 3
    Fours -> simpleNum dice 4
    Fives -> simpleNum dice 5
    Sixes -> simpleNum dice 6
    FullHouse -> fullHouse dice
    FourOfAKind -> fourOfAKind dice
    Choice -> sum dice
    LittleStraight -> littleStraight $ sort dice
    BigStraight -> bigStraight $ sort dice
    Yacht -> fiveOfAKind dice

simpleNum :: [Int] -> Int -> Int
simpleNum dice num = num * (numCount num dice)

numCount :: Int -> [Int] -> Int
numCount num dice = length (filter (==num) dice)

fullHouse :: [Int] -> Int
fullHouse dice = 
  if length (grouped dice) == 2 && length (head (grouped dice)) == 3 then sum dice
  else 0

fourOfAKind :: [Int] -> Int
fourOfAKind dice
  | length biggestGroup >= 4 = 4 * head biggestGroup
  | otherwise = 0
  where biggestGroup = head (grouped dice)

fiveOfAKind :: [Int] -> Int
fiveOfAKind dice 
  | all (== head dice) dice = 50
  | otherwise = 0

bigStraight :: [Int] -> Int
bigStraight dice
  | dice == [2,3,4,5,6] = 30
  | otherwise = 0

littleStraight :: [Int] -> Int
littleStraight dice
  | dice == [1,2,3,4,5] = 30
  | otherwise = 0

grouped :: [Int] -> [[Int]]
grouped [] = []
grouped dice = sortBy cmpLength $ group $ sort dice

cmpLength ::  [a] -> [a] -> Ordering
cmpLength b a   -- sort list of list in reverse order of length
  | length a > length b = GT 
  | length a < length b = LT 
  | otherwise = EQ

successive :: [Int] -> Bool
successive [] = True
successive [i] = True
successive (x:xs) = head xs == succ x && successive xs



main = do
  print $ "d1 ones: " ++ (show $ 1 == yacht Ones d1)
  print $ "d1 Twos: " ++ (show $ 4 == yacht Twos d1)
  print $ "ya Fours: " ++ (show $ 20 == yacht Fours ya)
  print $ "fh FullHouse: " ++ (show $ 12 == yacht FullHouse fh)
  print $ "ya FullHouse: " ++ (show $ 0 == yacht FullHouse ya)
  print $ "ya FourOfAKind: " ++ (show $ 16 == yacht FourOfAKind ya)
  print $ "fourk FourOfAKind: " ++ (show $ 20 == yacht FourOfAKind fourk)
  print $ "ya Yacht: " ++ (show $ 50 == yacht Yacht ya)
  print $ "fourk Yacht: " ++ (show $ 0 == yacht Yacht fourk)
  print $ "ya Choice: " ++ (show $ 20 == yacht Choice ya)
  print $ "Choice: " ++ (show $ 20 == yacht Choice ya)

  print $ "d2 BigStraight: " ++ (show $ 0 == yacht BigStraight d2)
  print $ "d7 BigStraight: " ++ (show $ 30 == yacht BigStraight d7)
  print $ "d1 BigStraight: " ++ (show $ 0 == yacht BigStraight d1)
  print $ "d6 BigStraight: " ++ (show $ 0 == yacht BigStraight d6)
  print $ "d7 LittleStraight: " ++ (show $ 0 == yacht LittleStraight d7)
  print $ "d6 LittleStraight: " ++ (show $ 30 == yacht LittleStraight d8)
  
  print("*****")

