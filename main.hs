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
    deriving (Show, Enum, Eq, Bounded)

  type ColorTriplet = (Color, Color, Color)

  colorValues :: [(Color, Integer)]
  colorValues = [(Black, 0), (Brown, 1), (Red, 2), (Orange, 3), (Yellow, 4),
      (Green, 5), (Blue, 6), (Violet, 7), (Grey, 8), (White, 9)]

  cMult :: [(Color, (Integer, String))]
  cMult = [(Black, (1, "ohms")), (Brown, (10, "ohms")), (Red, (100, "ohms")), 
      (Orange, (1, "kiloohms")), (Yellow, (10, "kiloohms")),
      (Green, (100, "kiloohms")), (Blue, (1, "megaohms")), 
      (Violet, (10, "megaohms")), 
      (Grey, (100, "megaohms")), 
      (White, (1, "gigaohms"))]

  newtype Resistor = Resistor { bands :: ColorTriplet } deriving Show

  label :: Resistor -> String
  label resistor = 
        let b = bands resistor in
        show (decodeTwo b * (multiplier b)) ++ " " ++ (units b)

  ohms :: Resistor -> Integer
  ohms resistor = 
        let b = bands resistor in
        (decodeTwo b) * 10^(resExp b)

  colorVal :: Color -> Integer -- color value of one band
  colorVal a = fromJust (lookup a colorValues)

  decodeTwo :: ColorTriplet -> Integer 
  decodeTwo (a, b, _) = 10*colorVal a + colorVal b

  resExp :: ColorTriplet -> Integer -- Exponent from third color band
  resExp (_, _, a) = colorVal a

  units :: ColorTriplet -> String -- Ohms, kilohms, megohms
  units (_, _, c) = fromJust $ fmap snd (lookup c cMult)

  multiplier :: ColorTriplet -> Integer -- 1, 10, or 100
  multiplier (_, _, c) = fromJust $ fmap fst (lookup c cMult)

---------- TESTING -------------------------
r1 = Resistor(Red, Blue, Yellow)
r2 = Resistor(Yellow, Black, Red)

q = lookup Green cMult 
v = decodeTwo (bands r1)
b = bands r1

main = do
  print v
  print("*****")

