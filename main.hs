-- Resistors 3
import Data.List 
import Data.Maybe

data Color =
    Black
  | Brown
  | Red
  | Orange
  | Yellow
  | Green
  | Blue
  | Violet
  | Grey
  | White
  deriving (Show, Enum, Bounded)

newType ColorTriplet = (Color, Color, Color)

colorValues :: [(Color, Integer)]
colorValues = [(Black, 0), (Brown, 1), (Red, 2), (Orange, 3), (Yellow, 4),
    (Green, 5), (Blue, 6), (Violet, 7), (Grey, 8), (White, 9)]

cMult :: [(Color, (Integer, String))]
cMult = [(Black, (1, "ohms")), (Brown, (10, "ohms")), (Red, (100, "ohms")), 
    (Orange, (1, "kiloohms")), (Yellow, (10, "kiloohms")),
    (Green, (100, "kiloohms")), (Blue, (1, "megohms")), 
    (Violet, (10, "megohms")), 
    (Grey, (100, "megohms")), 
    (White, (1, "gigohms"))]

newtype Resistor = Resistor { bands :: (Color, Color, Color) }
  deriving Show

label :: Resistor -> String
label resistor = error "You need to implement this function."

ohms :: Resistor -> Int
ohms resistor = error "You need to implement this function."

r1 = Resistor(Red, Blue, Yellow)

decodeTwo :: ColorTriplet -> Integer 
decodeTwo (a, b, _) = 10*colorVal a + colorVal b

colorVal :: Color -> Integer
colorVal a = fromJust (lookup a colorValues)

resNum :: ColorTriplet -> Integer
resNum cs = decodeTwo cs *  (multiplier cs)

units :: ColorTriplet -> String
units c = fmap snd (lookup c cMult)

multiplier :: ColorTriplet -> Integer
multiplier c = fmap fst (lookup c cMult)

q = lookup Green cMult 
v = decodeTwo inputColors 

main = do
  print (decodeTwo inputColors)
  print("*****")

