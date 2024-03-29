module LList where

infix 5 :>
data List a = Empty | a :> (List a) deriving (Show, Read, Eq, Ord)

toList :: [a] -> List a
toList [x] = x :> Empty
toList (x:xs) = x :> toList xs

fromList :: List a -> [a]
fromList Empty = []
fromList (x:>xs) = x : fromList xs

head :: List a -> a
head Empty = error "Can't take head of empty list"
head (x:>xs) = x

datum :: a -> List a
datum x = x :> Empty

last :: List a -> a
last Empty = error "Can't take last of empty list"
last (x:>Empty) = x
last (x:>xs) = LList.last xs

init :: List a -> List a
init Empty = error "Can't take init of empty list"
init (x:>Empty) = Empty
init (x:>xs) = x :> LList.init xs

infix 5 !+
(!+) :: List a -> List a -> List a
Empty !+ y = y
(x:>xs) !+ y = x :> (xs !+ y) 

reverseLinkedList :: List a -> List a
reverseLinkedList Empty = Empty
reverseLinkedList (x :> Empty) = x :> Empty
reverseLinkedList xs = LList.last xs :> reverseLinkedList (LList.init xs)

v = toList ["cat","dog","owl"]
w = [1,2,3]

r = reverseLinkedList

main = do 
    print "***"

