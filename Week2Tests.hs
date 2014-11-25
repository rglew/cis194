module Week2Tests where

import Test.HUnit
import Week2


week2Tests = runTestTT week2Ex1Tests

test2_1_1 = TestCase (assertEqual "formableBy" (True) (formableBy "fun" ['x','n','i','f','u','e','l']))
test2_1_2 = TestCase (assertEqual "formableBy" (True) (formableBy "haskell" ['k','l','e','h','a','l','s']))
test2_1_3 = TestCase (assertEqual "formableBy" (False) (formableBy "haskell" ['k','l','e','h','a','y','s']))

week2Ex1Tests = TestList [TestLabel "week2Ex1Tests" test2_1_1, test2_1_2, test2_1_3]