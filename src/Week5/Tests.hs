module Week5.Tests where

import Week5.Week5
import Week5.Ring
import Week5.Parser
import Test.Tasty
import Test.Tasty.HUnit

week5UnitTests = testGroup "Week 5 Unit tests"
  [ 
    testCase "Ex 01a - Integer Ring has Additive Identity" $
       1 @=? add (1 :: Integer) addId,

    testCase "Ex 01b - Integer Ring is commutative" $
       add (1 :: Integer) (3 :: Integer) @=? add (3 :: Integer) (1 :: Integer),

    testCase "Ex 01c - Integer Ring is associative" $
        ((1 :: Integer) `mul` 3) `mul` 2 @=? 1 `mul` (3 `mul` 2), -- only have to give the compiler one hint :)

    testCase "Ex 01d - Integer Ring has Additive Inverse " $
       (0 :: Integer) @=? add 1 (addInv 1),

    testCase "Ex 01e - Ring Integer is parsable" $
       Just (25 :: Integer) @=? parseRing "1 + 2 * 3 * 4" ,

    testCase "Ex 02a - Mod5 Ring has Additive Identity" $
       (Mod5 1) @=? add (Mod5 1) addId,

    testCase "Ex 02b - Mod5 Ring is commutative" $
       add (Mod5 1) (Mod5 3) @=? add (Mod5 3) (Mod5 1),

    testCase "Ex 02c - Mod5 Ring is associative" $
        ((Mod5 1) `mul` (Mod5 3)) `mul` (Mod5 2) @=? (Mod5 1) `mul` ((Mod5 3) `mul` (Mod5 2)),

    testCase "Ex 02d - Mod5 Ring has Additive Inverse " $
       (Mod5 (-1)) @=? addInv (Mod5 1),

    testCase "Ex 02e - Mod5 is parsable" $
       Just (Mod5 0) @=? (parseRing "1 + 2 * 3 * 4"),

    testCase "Ex 03a - Mat2x2 Ring has Additive Identity" $
       (Mat2x2 5 5 5 5) @=? add (Mat2x2 5 5 5 5) addId,

    testCase "Ex 03b - Mat2x2 Ring is commutative" $
       add (Mat2x2 5 5 5 5) (Mat2x2 3 3 3 3) @=? add (Mat2x2 3 3 3 3) (Mat2x2 5 5 5 5),

    testCase "Ex 03c - Mat2x2 Ring is associative" $
        ((Mat2x2 1 1 1 1) `mul` (Mat2x2 3 3 3 3)) `mul` (Mat2x2 2 2 2 2) @=? (Mat2x2 1 1 1 1) `mul` ((Mat2x2 3 3 3 3) `mul` (Mat2x2 2 2 2 2)),

    testCase "Ex 03d - Mat2x2 Ring has Additive Inverse " $
       (Mat2x2 5 5 5 5) @=? addInv (Mat2x2 (-5) (-5) (-5) (-5)),

    testCase "Ex 03e - Mat2x2 is parsable" $
       Just (Mat2x2 18 18 27 27) @=? (parseRing "[[2,2][3,3]] * [[4,4][5,5]]")
 
  ]