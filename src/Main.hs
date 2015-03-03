module Main where

import Week1.Tests
import Week2.Tests
import Week3.Tests
import Week4.Tests
import Week5.Tests
import Test.Tasty
import Test.Tasty.HUnit



main = do week1Tests 
          week2Tests
          tastyTests


tastyTests = defaultMain tests

tests :: TestTree
tests = testGroup "Tasty Unit Tests" [week3UnitTests, week4UnitTests, week5UnitTests]