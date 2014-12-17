module Week2.Tests where

import Test.HUnit
import Week2.Week2


week2Tests = do runTestTT week2Ex1Tests
                runTestTT week2Ex2Tests
                runTestTT week2Ex3Tests
                runTestTT week2Ex4Tests
                runTestTT week2Ex5Tests
                runTestTT week2Ex6Tests
                runTestTT week2Ex7Tests

test2_1_1 = TestCase $ True  @=?  formableBy "fun" ['x','n','i','f','u','e','l']
test2_1_2 = TestCase $ True  @=? formableBy "haskell" ['k','l','e','h','a','l','s']
test2_1_3 = TestCase $ False @=? formableBy "haskell" ['k','l','e','h','a','y','s']

test2_2_1 = TestCase $ ["ab","ad","ba","bad","cab","cad","dab"] @=? wordsFrom ['a','b','c','d']
test2_2_2 = TestCase $ ["eh","el","ell","he","hell","hello","helo","ho","hoe","hole","lo","oe","oh","ole"] @=? wordsFrom ['h','e','l','l','o']

test2_3_1 = TestCase $ True @=? wordFitsTemplate "??r?" ['c','x','e','a','b','c','l'] "care"
test2_3_2 = TestCase $ False @=? wordFitsTemplate "??r?" ['c','x','e','w','b','c','l'] "care"
test2_3_3 = TestCase $ False @=? wordFitsTemplate "??r?" ['c','x','e','a','b','c','l'] "car"
test2_3_4 = TestCase $ True @=? wordFitsTemplate "let" ['x','x'] "let"

test2_4_1 = TestCase $ ["acre","bare","carb","care","carl","earl"] @=? wordsFittingTemplate "??r?" ['c','x','e','a','b','c','l']

test2_5_1 = TestCase $ 6 @=? scrabbleValueWord "care"
test2_5_2 = TestCase $ 22 @=? scrabbleValueWord "quiz"

-- Suspect bug in case 2.6.2 where order of result was reversed 
test2_6_1 = TestCase $ ["carb"] @=? bestWords (wordsFittingTemplate "??r?" ['c','x','e','a','b','c','l'])
test2_6_2 = TestCase $ ["cat","bat"] @=? bestWords ["cat", "rat", "bat"]
test2_6_3 = TestCase $[] @=? bestWords []

-- Bug in this test in the homework, results for case 1 & 3 were mixed up
test2_7_1 = TestCase $ 11 @=? scrabbleValueTemplate "?e??3" "peace"
test2_7_2 = TestCase $ 24 @=? scrabbleValueTemplate "De?2?" "peace"
test2_7_3 = TestCase $ 27 @=? scrabbleValueTemplate "??Tce" "peace"


week2Ex1Tests = TestList [TestLabel "week2Ex1Tests" test2_1_1, test2_1_2, test2_1_3]
week2Ex2Tests = TestList [TestLabel "week2Ex2Tests" test2_2_1, test2_2_2]
week2Ex3Tests = TestList [TestLabel "week2Ex3Tests" test2_3_1, test2_3_2, test2_3_3, test2_3_4]
week2Ex4Tests = TestList [TestLabel "week2Ex4Tests" test2_4_1]
week2Ex5Tests = TestList [TestLabel "week2Ex5Tests" test2_5_1, test2_5_2]
week2Ex6Tests = TestList [TestLabel "week2Ex6Tests" test2_6_1, test2_6_2, test2_6_3]
week2Ex7Tests = TestList [TestLabel "week2Ex7Tests" test2_7_1, test2_7_2, test2_7_3]
