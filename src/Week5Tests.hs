module Week5Tests where

import Week5.Tests
import Test.Tasty
import Test.Tasty.HUnit



main = do tastyTests


tastyTests = defaultMain tests

tests :: TestTree
tests = testGroup "Tasty Week 5 Unit Tests" [week5UnitTests]