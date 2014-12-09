module Week3Tests where

import Week3
import Log
import Test.Tasty
import Test.Tasty.HUnit

week3Tests = defaultMain tests

tests :: TestTree
tests = testGroup "Week 3 Tests" [unitTests]

unitTests = testGroup "Week 3 Unit tests"
  [ testCase "Parse error message" $
       ValidLM (LogMessage (Error 2) 562 "help help") @=? parseMessage "E 2 562 help help"

  , testCase  "Parse Log Message" $
      ValidLM (LogMessage Info 29 "la la la") @=? parseMessage "I 29 la la la"

  , testCase  "Parse Garbage Message" $
      InvalidLM "This is not in the right format" @=? parseMessage "This is not in the right format"

  ]