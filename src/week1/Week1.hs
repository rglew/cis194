module Week1.Week1 where

lastDigit :: Integer -> Integer
lastDigit x = x `mod` 10 

dropLastDigit :: Integer -> Integer
dropLastDigit x = x `div` 10

toDigits :: Integer -> [Integer]
toDigits x
    | x < 1 = []
    | otherwise = (toDigits (dropLastDigit x)) ++ [lastDigit x]

doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther x = reverse $ double $ reverse x where
  double = zipWith ($) (cycle [id,(*2)])

sumDigits :: [Integer] -> Integer
sumDigits x = sum $ concatMap toDigits x

validate :: Integer -> Bool
validate x
    | ((sumDigits $ doubleEveryOther $ toDigits x) `mod` 10) == 0 = True
    | otherwise = False

type Peg = String
type Move = (Peg, Peg)
hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi 0 _ _ _ = []
hanoi 1 a b _ = [(a, b)]
hanoi n a b c = (hanoi (n-1) a c b) ++ (hanoi 1 a b c) ++ (hanoi (n-1) c b a)

