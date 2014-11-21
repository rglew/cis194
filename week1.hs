module Week1 where

lastDigit :: Integer -> Integer
lastDigit x = x `mod` 10 

dropLastDigit :: Integer -> Integer
dropLastDigit x = x `div` 10

toDigits :: Integer -> [Integer]
toDigits x
	| x < 0 = []
	| x < 10 = [x]
	| otherwise = (toDigits (dropLastDigit x)) ++ [lastDigit x]