module Main where

import Test.HUnit
import Week1 


main =  runTestTT week1Tests

test1_1 = TestCase (assertEqual "lastDigit" (3) (lastDigit 123))
test1_2 = TestCase (assertEqual "lastDigit" (0) (lastDigit 0))
test1_3 = TestCase (assertEqual "dropLastDigit" (12) (dropLastDigit 123))
test1_4 = TestCase (assertEqual "dropLastDigit" (0) (dropLastDigit 5))

test2_1 = TestCase (assertEqual "toDigits" ([1,2,3,4]) (toDigits 1234))
test2_2 = TestCase (assertEqual "toDigits" ([]) (toDigits 0))
test2_3 = TestCase (assertEqual "toDigits" ([]) (toDigits (-17)))

week1Tests = TestList [TestLabel "week1Tests" test1_1, test1_2, test1_3, test1_4, test2_1, test2_2, test2_3]
