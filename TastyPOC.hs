module TastyPOC where

import Test.Tasty
import Test.Tasty.SmallCheck as SC
import Test.Tasty.QuickCheck as QC
import Test.Tasty.HUnit
import Week1

import Data.List
import Data.Ord

tastyPOC = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [unitTests, properties]

properties :: TestTree
properties = testGroup "Properties" [scProps, qcProps]

scProps = testGroup "(checked by SmallCheck)"
  [ SC.testProperty "sort == sort . reverse" $
      \list -> sort (list :: [Int]) == sort (reverse list)
  , SC.testProperty "Fermat's little theorem" $
      \x -> ((x :: Integer)^7 - x) `mod` 7 == 0
  -- the following property does not hold
  , SC.testProperty "Fermat's last theorem" $
      \x y z n ->
        (n :: Integer) >= 3 SC.==> x^n + y^n /= (z^n :: Integer)
  ]

qcProps = testGroup "(checked by QuickCheck)"
  [ QC.testProperty "sort == sort . reverse" $
      \list -> sort (list :: [Int]) == sort (reverse list)
  , QC.testProperty "Fermat's little theorem" $
      \x -> ((x :: Integer)^7 - x) `mod` 7 == 0
  -- the following property does not hold
  , QC.testProperty "Fermat's last theorem" $
      \x y z n ->
        (n :: Integer) >= 3 QC.==> x^n + y^n /= (z^n :: Integer)
  ]


unitTests = testGroup "Unit tests"
  [ testCase "See if lastDigit works" $
      3 @=? lastDigit 123

  -- the following test does not hold
  , testCase "Try it with 0" $
      0 @=? lastDigit 0
  ]