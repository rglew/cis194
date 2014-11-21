module Main where

import Test.HUnit
import Week1 


main =  do runTestTT week1Ex1Tests
           runTestTT week1Ex2Tests
           runTestTT week1Ex3Tests
           runTestTT week1Ex4Tests
           runTestTT week1Ex5Tests
           runTestTT week1Ex6Tests

test1_1 = TestCase (assertEqual "lastDigit" (3) (lastDigit 123))
test1_2 = TestCase (assertEqual "lastDigit" (0) (lastDigit 0))
test1_3 = TestCase (assertEqual "dropLastDigit" (12) (dropLastDigit 123))
test1_4 = TestCase (assertEqual "dropLastDigit" (0) (dropLastDigit 5))

test2_1 = TestCase (assertEqual "toDigits" ([1,2,3,4]) (toDigits 1234))
test2_2 = TestCase (assertEqual "toDigits" ([]) (toDigits 0))
test2_3 = TestCase (assertEqual "toDigits" ([]) (toDigits (-17)))

test3_1 = TestCase (assertEqual "doubleEveryOther" ([16,7,12,5]) (doubleEveryOther [8,7,6,5]))
test3_2 = TestCase (assertEqual "doubleEveryOther" ([1,4,3]) (doubleEveryOther [1,2,3]))

test4_1 = TestCase (assertEqual "sumDigits" (22) (sumDigits [16,7,12,5]))

test5_1 = TestCase (assertEqual "validate" (True) (validate 4012888888881881))
test5_2 = TestCase (assertEqual "validate" (False) (validate 4012888888881882))

test6_1 = TestCase (assertEqual "hanoi" ([("a","c"), ("a","b"), ("c","b")]) (hanoi 2 "a" "b" "c"))

week1Ex1Tests = TestList [TestLabel "week1Ex1Tests" test1_1, test1_2, test1_3, test1_4]
week1Ex2Tests = TestList [TestLabel "week1Ex2Tests" test2_1, test2_2, test2_3]
week1Ex3Tests = TestList [TestLabel "week1Ex3Tests" test3_1, test3_2]
week1Ex4Tests = TestList [TestLabel "week1Ex4Tests" test4_1]
week1Ex5Tests = TestList [TestLabel "week1Ex5Tests" test5_1, test5_2]
week1Ex6Tests = TestList [TestLabel "week1Ex6Tests" test6_1]
