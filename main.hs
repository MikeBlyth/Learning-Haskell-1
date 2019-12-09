module Robot where

type RobotPos = (Int, Int, Int)

moveRobot :: RobotPos -> String -> RobotPos
moveRobot pos [] = pos
moveRobot pos (x:xs) = moveRobot (newPos pos x) xs

newPos :: RobotPos -> Char -> RobotPos
newPos (rotation, x, y) c = case c of
    'L' -> (rotation+90, x,y)
    'R' -> (rotation-90, x,y)
    'A' -> advance (rotation, x, y)
    _ -> (rotation, x, y)

advance :: RobotPos -> RobotPos -- will only work with multiples of 90 degrees, since sin and cos are rounded
advance (rotation, x, y) =
  let rads = fromIntegral rotation * pi/180
      sinr = round $ sin rads
      cosr = round $ cos rads
  in (rotation, x + cosr, y+sinr)

main = do 
    print "***"




{-
import Control.Monad
import Control.Monad.State

a = [1,2,3]
a2 = [4,5,6]
b = ["cat", "dog", "mouse"]

f1 a = Just (show a)
f2 a = Just (a ++ "!")

type Stack = [Int] 

pop :: State Stack Int  
pop = State $ \(x:xs) -> (x,xs)  
  
push :: Int -> State Stack ()  
push a = State $ \xs -> ((),a:xs) 

main = do
  print("*****")
-}