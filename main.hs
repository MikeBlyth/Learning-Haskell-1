{-The ISBN-10 format is 9 digits (0 to 9) plus one check character (either a digit or an X only). In the case the check character is an X, this represents the value '10'. These may be communicated with or without hyphens, and can be checked for their validity by the following formula:

(x1 * 10 + x2 * 9 + x3 * 8 + x4 * 7 + x5 * 6 + x6 * 5 + x7 * 4 + x8 * 3 + x9 * 2 + x10 * 1) mod 11 == 0
If the result is 0, then it is a valid ISBN-10, otherwise it is
invalid.-}


check :: String -> Bool
check s 
  | not $ all (`elem` (validDigits ++ " -")) s = False
  | length digits /= 10 = False
  | elem 'X' (init digits) = False
  | otherwise = 0 == mod (sum $ zipWith (*) mults (map val digits)) 11
  where validDigits =  "0123456789X"
        digits = filter (`elem` validDigits) s 
        mults = [10,9..1]

val :: Char -> Int
val c
  | c `elem` ['0' .. '9'] = read [c] :: Int
  | c == 'X' = 10
  | otherwise = error "program error - val should not receive invalid character"

main = do
  print (check "98245726788")
  print("*****")

