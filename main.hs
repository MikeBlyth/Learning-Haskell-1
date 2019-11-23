-- Resistors 3
import Data.List 

type Color = String

colorValues :: [(Color, Integer)]
colorValues = [("Black", 0), ("Brown", 1), ("Red", 2), ("Orange", 3), ("Yellow", 4),
    ("Green", 5), ("Blue", 6), ("Violet", 7), ("Grey", 8), ("White", 9)]

cMult :: [(Color, (Integer, String))]
cMult = [("Black", (1, "ohms")), ("Brown", (10, "ohms")), ("Red", (100, "ohms")), 
    ("Orange", (1, "kiloohms")), ("Yellow", (10, "kiloohms")),
    ("Green", (100, "kiloohms")), ("Blue", (1, "megohms")), 
    ("Violet", (10, "megohms")), 
    ("Grey", (100, "megohms")), 
    ("White", (1, "gigohms"))]

inputColors = ["Red", "Blue", "Yellow"]

decodeTwo :: [Color] -> Maybe Integer 
decodeTwo [] = Nothing
decodeTwo [""] = Nothing
decodeTwo [x] = Nothing
decodeTwo (a:b:cs) = fmap (+) (fmap (10*) (colorVal a)) <*> (colorVal b)

colorVal :: Color -> Maybe Integer
colorVal a = lookup a colorValues 

resNum :: Maybe Integer -> Color -> Maybe Integer
resNum a b = fmap (*) a <*> (multiplier b)

units :: Color -> Maybe String
units c = fmap snd (lookup c cMult)

multiplier :: Color -> Maybe Integer
multiplier c = fmap fst (lookup c cMult)

q = lookup "Green" cMult 
v = decodeTwo inputColors 

main = do
  print (decodeTwo inputColors)
  print("*****")

