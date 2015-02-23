module Week4.Week4 where

import Data.Char
import Data.List
import Data.Maybe
import Data.String.Utils
import Week4.BST

ex1 :: a -> b -> b
ex1 _ b = b
--must return b - only option

ex2 :: a -> a -> a
ex2 y z = z
-- could return either a or some operation between them

ex3 :: Int -> a -> a
ex3 a b = b
-- to be total must return a, you may not have a type that interacts with Int

ex4 :: Bool -> a -> a -> a
ex4 tf v1 v2
  | tf == True = v1
  | tf == False = v2
-- could do a number of things...

ex5 :: Bool -> Bool
ex5 trueOrFalse
  | (trueOrFalse == True) = False
  | (trueOrFalse == False) = True

ex6 :: (a -> a) -> a
ex6 x = error "Don't see how this can be written"

ex7 :: (a -> a) -> a -> a
ex7 f x = f x
-- simply applies f to x

ex8 :: [a] -> [a]
ex8 a = a

ex9 :: (a -> b) -> [a] -> [b]
ex9 a b = error "Not possible"

-- getting something from a Maybe needs to account for the Nothing condition.  To do this you need to supply a default value of some specific type (check out fromMaybe in Data.Maybe)
ex10 :: Maybe a -> a
ex10 x = error "No can do"

ex11 :: a -> Maybe a
ex11 c = Just c

ex12 :: Maybe a -> Maybe a
ex12 a = a

insertBST :: (a -> a -> Ordering) -> a -> BST a -> BST a
insertBST _ x Leaf = Node Leaf x Leaf
insertBST f x (Node a y b)
    | f x y == EQ || f x y == LT = Node (insertBST f x a) y b
    | f x y == GT                = Node a y (insertBST f x b)
insertBST _ _ _ = error "impossible"

allCaps :: [String] -> Bool
allCaps s = foldl (&&) True $ map fA s 
            where fA s 
                      | s == "" = False
                      | otherwise = isUpper (s!!0)

dropTrailingWhitespace :: String -> String
dropTrailingWhitespace s = rstrip s

firstLetters :: [String] -> [Char]
firstLetters ss = map fromJust $ filter isJust $ map safeHead ss

safeHead :: [a] -> Maybe a
safeHead [] = Nothing
safeHead (x:_) = Just x

asList :: [String] -> String
asList ss = "[" ++ intercalate "," ss ++ "]"
