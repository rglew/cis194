module Week3.Tests where

import Week3.Week3
import Week3.Log
import Test.Tasty
import Test.Tasty.HUnit

testLog :: String
testLog = "I 5053 pci_id: con ing!\nI 4681 ehci 0xf43d000:15: regista14: [0xbffff 0xfed nosabled 00-02] Zonseres: brips byted nored)\nW 3654 e8] PGTT ASF! 00f00000003.2: 0x000 - 0000: 00009dbfffec00000: Pround/f1743colled\nI 4076 verse.'\nI 4764 He trusts to you to set them free,"

testFullLog :: [LogMessage]
testFullLog = [LogMessage Info 5053 "pci_id: con ing!",LogMessage Info 4681 "ehci 0xf43d000:15: regista14: [0xbffff 0xfed nosabled 00-02] Zonseres: brips byted nored)",LogMessage Info 4076 "verse.'",LogMessage Info 4764 "He trusts to you to set them free,"]

week3UnitTests = testGroup "Week 3 Unit tests"
  [ testCase "Parse error message" $
       ValidLM (LogMessage (Error 2) 562 "help help") @=? parseMessage "E 2 562 help help"

  , testCase  "Parse Log Message" $
      ValidLM (LogMessage Info 29 "la la la") @=? parseMessage "I 29 la la la"

  , testCase "Check Parse Function" $
      [LogMessage Info 5053 "pci_id: con ing!",LogMessage Info 4681 "ehci 0xf43d000:15: regista14: [0xbffff 0xfed nosabled 00-02] Zonseres: brips byted nored)",LogMessage Info 4076 "verse.'",LogMessage Info 4764 "He trusts to you to set them free,"] @=? parse testLog

  , testCase  "Parse Garbage Message" $
      InvalidLM "This is not in the right format" @=? parseMessage "This is not in the right format"

  , testCase "Check valid messages only" $
      [LogMessage Info 5053 "pci_id: con ing!",LogMessage Info 4681 "ehci 0xf43d000:15: regista14: [0xbffff 0xfed nosabled 00-02] Zonseres: brips byted nored)",LogMessage Info 4076 "verse.'",LogMessage Info 4764 "He trusts to you to set them free,"] @=? (validMessagesOnly $ map parseMessage $ lines testLog)

  , testCase "Check message timestamps - LT case" $ 
      LT @=? compareMsgs (LogMessage Warning 153 "Not a speck of light is showing, so the danger must be growing...") (LogMessage Info 208 "the Weighted Companion Cube cannot talk")

  , testCase "Check message timestamps - EQ case" $ 
      EQ @=? compareMsgs (LogMessage (Error 101) 2001 "My God! It’s full of stars!") (LogMessage Info 2001 "Daisy, Daisy, give me your answer do.")

  , testCase "Check sorting" $
     ([LogMessage Info 4076 "verse.'",LogMessage Info 4681 "ehci 0xf43d000:15: regista14: [0xbffff 0xfed nosabled 00-02] Zonseres: brips byted nored)",LogMessage Info 4764 "He trusts to you to set them free,",LogMessage Info 5053 "pci_id: con ing!"]) @=? sortMessages testFullLog
 
  -- This uses IO() in the test so won't work here... need to redo it without reading from a file 
  --, testCase "Test What Went Wrong" $
  --   "Way too many pickles\nBad pickle-flange interaction detected\nFlange failed!" @=? testWhatWentWrong parse whatWentWrong "sample.log"
  ]