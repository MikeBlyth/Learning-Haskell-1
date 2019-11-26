{-The ISBN-10 format is 9 digits (0 to 9) plus one check character (either a digit or an X only). In the case the check character is an X, this represents the value '10'. These may be communicated with or without hyphens, and can be checked for their validity by the following formula:

(x1 * 10 + x2 * 9 + x3 * 8 + x4 * 7 + x5 * 6 + x6 * 5 + x7 * 4 + x8 * 3 + x9 * 2 + x10 * 1) mod 11 == 0
If the result is 0, then it is a valid ISBN-10, otherwise it is
invalid.-}

import Data.Maybe

i = "3-598-21508-8"  -- for testing
idig = digits i      -- for testing

mults = [10,9..1]

digits :: String -> String
digits s = filter (\x -> x `elem` "0123456789X") s

val :: Char -> Int
val c
  | c `elem` ['0' .. '9'] = read [c] :: Int
  | c == 'X' = 10

check :: String -> Bool
check s = 0 == mod (sum $ zipWith (*) mults (map val (digits s))) 11

main = do
  print (check i)
  print("*****")

