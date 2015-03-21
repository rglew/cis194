module Week6.Tests where

import Week6.Week6
import Test.Tasty
import Test.Tasty.HUnit
import Data.Aeson
import Data.Monoid
import GHC.Generics
import qualified Data.ByteString.Lazy.Char8 as B
import qualified Data.Text as T
import qualified Data.Text.IO as T

trueVal = T.pack "Y"
falseVal = T.pack "N"
fooVal = T.pack "Foo"

week6UnitTests = testGroup "Week 6 Unit tests" --not sure if this will work but lets see
  [ 
    testCase "Ex 01a - Test N converts to False value" $
       (Bool False) @=? ynToBool (String falseVal), 

    testCase "Ex 01b - Test Y converts to True value" $
       (Bool True) @=? ynToBool (String trueVal), 

    testCase "Ex 01c - Test other input leaves value alone" $
       (String  fooVal) @=? ynToBool (String fooVal) 
  ]