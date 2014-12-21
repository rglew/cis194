module Week4.Week4 where

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
ex4 a b c = b
-- could do a number of things...

ex5 :: Bool -> Bool
ex5 trueOrFalse = trueOrFalse
-- pretty obvious

ex6 :: (a -> a) -> a
ex6 a = undefined

ex7 :: (a -> a) -> a -> a
ex7 a b = undefined

ex8 :: [a] -> [a]
ex8 a = a

ex9 :: (a -> b) -> [a] -> [b]
ex9 a b = undefined

-- getting something from a Maybe needs to account for the Nothing condition.  To do this you need to supply a default value of some specific type (check out fromMaybe in Data.Maybe)
ex10 :: Maybe a -> a
ex10 x = error "No can do"

ex11 :: a -> Maybe a
ex11 c = Just c

ex12 :: Maybe a -> Maybe a
ex12 a = a




 