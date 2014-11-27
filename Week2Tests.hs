module Week2Tests where

import Test.HUnit
import Week2


week2Tests = do runTestTT week2Ex1Tests
                runTestTT week2Ex2Tests
                runTestTT week2Ex3Tests

test2_1_1 = TestCase (assertEqual "formableBy" (True) (formableBy "fun" ['x','n','i','f','u','e','l']))
test2_1_2 = TestCase (assertEqual "formableBy" (True) (formableBy "haskell" ['k','l','e','h','a','l','s']))
test2_1_3 = TestCase (assertEqual "formableBy" (False) (formableBy "haskell" ['k','l','e','h','a','y','s']))

test2_2_1 = TestCase (assertEqual "wordsFrom" ( ["ab","ad","ba","bad","cab","cad","dab"]) (wordsFrom ['a','b','c','d']))
test2_2_2 = TestCase (assertEqual "wordsFrom" ( ["eh","el","ell","he","hell","hello","helo","ho","hoe","hole","lo","oe","oh","ole"]) (wordsFrom ['h','e','l','l','o']))

test2_3_1 = TestCase (assertEqual "wordFitsTemplate" (True) (wordFitsTemplate "??r?" ['c','x','e','a','b','c','l'] "care"))
test2_3_2 = TestCase (assertEqual "wordFitsTemplate" (False) (wordFitsTemplate "??r?" ['c','x','e','w','b','c','l'] "care"))
test2_3_3 = TestCase (assertEqual "wordFitsTemplate" (False) (wordFitsTemplate "??r?" ['c','x','e','a','b','c','l'] "car"))
test2_3_4 = TestCase (assertEqual "wordFitsTemplate" (True) (wordFitsTemplate "let" ['x','x'] "let"))


week2Ex1Tests = TestList [TestLabel "week2Ex1Tests" test2_1_1, test2_1_2, test2_1_3]
week2Ex2Tests = TestList [TestLabel "week2Ex2Tests" test2_2_1, test2_2_2]
week2Ex3Tests = TestList [TestLabel "week2Ex3Tests" test2_3_1, test2_3_2, test2_3_3, test2_3_4]