-- Isogram
import Data.List

s = "isogramyup"
t = "isogramnope"

isogram :: String -> Bool
isogram x = nub x == x

main = do
  print("*****")

