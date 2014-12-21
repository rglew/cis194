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

  , testCase "Ex 3a" $
      2 @=? ex3 2 2

  , testCase "Ex 3b" $
      '2' @=? ex3 2 '2'

  , testCase "Ex 4a" $
      "Freddy" @=? ex4 True "Freddy" "Jane"

  , testCase "Ex 4b" $
      "Jane" @=? ex4 False "Freddy" "Jane"

  , testCase "Ex 5a" $
      True @=? ex5 False

  , testCase "Ex 7" $
      "foobar" @=? ex7 (++"bar") ("foo")


  ]