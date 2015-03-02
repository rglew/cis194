module Week4Tests where

import Week4.Tests
import Test.Tasty
import Test.Tasty.HUnit



main = do tastyTests


tastyTests = defaultMain tests

tests :: TestTree
tests = testGroup "Tasty Week 4 Unit Tests" [week4UnitTests]