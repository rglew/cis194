module Week4.Tests where

import Week4.Week4
import Test.Tasty
import Test.Tasty.HUnit


week4UnitTests = testGroup "Week 4 Unit tests"
  [ testCase "Ex1 Int" $
       2 @=? ex1 1 2

  , testCase  "Ex1 Char" $
      "b" @=? ex1 "a" "b"

  , testCase "Ex 2a" $
      (Just 4) @=? ex2 (Nothing) (Just 4)

  , testCase "Ex 2b" $
      4 @=? ex2 2 4

  ]