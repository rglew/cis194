module Week1 where

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

{- What if we can rewrite our recurrence in the following fashion:

Tn=Tn−1+1+Tn−1

This tells us that to solve the tower for n disks (which will take Tn steps) we need to sove the tower for n−1 disks, then solve the tower for 1 disk and then solve the tower for n−1 disks again. You can probably already see where I am going with this, there is only a slight intuitive leap we need to make here, regarding where to transfer the disks each time. We need to transfer n−1 disks from the source peg to the pivot peg, then transfer 1 disk from the source peg to the destination peg and then transfer n−1 disks from the pivot to the destination and we're done. 

To move n discs (stacked in increasing size) from peg a to peg b
using peg c as temporary storage,
1. move n − 1 discs from a to c using b as temporary storage
2. move the top disc from a to b
3. move n − 1 discs from c to b using a as temporary storage.

-}