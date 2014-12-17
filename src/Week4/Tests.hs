module Week4.Tests where

import Week4.Week4
import Test.Tasty
import Test.Tasty.HUnit

week4Tests = defaultMain tests

tests :: TestTree
tests = testGroup "Week 4 Tests" [unitTests]

unitTests = testGroup "Week 4 Unit tests"
  [ testCase "Ex1 Int" $
       2 @=? ex1 1 2

  , testCase  "Ex1 Char" $
      "b" @=? ex1 "a" "b"
  ]