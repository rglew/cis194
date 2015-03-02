module Week4.Tests where

import Week4.Week4
import Week4.BST
import Test.Tasty
import Test.Tasty.HUnit

bst = Node (Node Leaf 2 (Node Leaf 3 Leaf)) 4 (Node (Node Leaf 5 Leaf) 6 Leaf)
thing = Node Leaf 4 Leaf

week4UnitTests = testGroup "Week 4 Unit tests"
  [ testCase "Ex 01a" $
       2 @=? ex1 1 2

  , testCase  "Ex 01b" $
      "b" @=? ex1 "a" "b"

  , testCase "Ex 02a" $
      (Just 4) @=? ex2 (Nothing) (Just 4)

  , testCase "Ex 02b" $
      4 @=? ex2 2 4

  , testCase "Ex 03a" $
      2 @=? ex3 2 2

  , testCase "Ex 03b" $
      '2' @=? ex3 2 '2'

  , testCase "Ex 04a" $
      "Freddy" @=? ex4 True "Freddy" "Jane"

  , testCase "Ex 04b" $
      "Jane" @=? ex4 False "Freddy" "Jane"

  , testCase "Ex 05a" $
      True @=? ex5 False

  , testCase "Ex 07a" $
      "foobar" @=? ex7 (++"bar") ("foo")

  , testCase "Ex 08a" $
      ["foo"] @=? ex8 ["foo"]

  , testCase "Ex 11a" $
      (Just 4) @=? ex11 4

  , testCase "Ex 12a" $
      (Just 4) @=? ex12 (Just 4)

  , testCase "Ex 12b" $
      Nothing @=? ex12 (Nothing :: Maybe Int) -- Have to give the compiler a clue so it knows what to show!

  , testCase "Ex 14a" $
      True @=? allCaps ["Hi","There"]

  , testCase "Ex 14b" $
      True @=? allCaps []

  , testCase "Ex 14c" $
      False @=? allCaps ["", "Blah"]

  , testCase "Ex 14d" $
      False @=? allCaps ["Hi","there"]
 
  , testCase "Ex 15a" $
      "foo" @=? dropTrailingWhitespace "foo"

  , testCase "Ex 15b" $
      "" @=? dropTrailingWhitespace ""

  , testCase "Ex 15c" $
      "bar" @=? dropTrailingWhitespace "bar  "

  , testCase "Ex 16a" $
      ['f','b'] @=? firstLetters ["foo", "bar"]

  , testCase "Ex 16b" $
      ['a'] @=? firstLetters ["alpha",""]

  , testCase "Ex 16c" $
      [] @=? firstLetters []

  , testCase "Ex 16d" $
      [] @=? firstLetters ["",""]

  , testCase "Ex 17a" $
    "[alpha,beta,gamma]" @=? asList ["alpha","beta","gamma"]

  , testCase "Ex 17b" $
    "[]" @=? asList []

  , testCase "Ex 17c" $
    "[lonely]" @=? asList ["lonely"]

  ]