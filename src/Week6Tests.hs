module Week6Tests where

import Week6.Tests
import Test.Tasty
import Test.Tasty.HUnit



main = do tastyTests


tastyTests = defaultMain tests

tests :: TestTree
tests = testGroup "CIS194 Week 6 Unit Tests" [week6UnitTests]