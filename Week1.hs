module Week1 where

lastDigit :: Integer -> Integer
lastDigit x = x `mod` 10 

dropLastDigit :: Integer -> Integer
dropLastDigit x = x `div` 10

toDigits :: Integer -> [Integer]
toDigits x
	| x < 1 = []
	| x < 10 = [x]
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
hanoi = undefined