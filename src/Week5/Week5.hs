module Week5.Week5 where

import Week5.Ring
import Week5.Parser
import Data.List


data Mod5 = Mod5 Integer deriving (Show, Eq)

instance Ring Mod5 where
  addId = Mod5 0
  addInv (Mod5 a) = Mod5 $ negate a
  mulId = Mod5 1
  add (Mod5 a) (Mod5 b) = Mod5 ( (a + b)  `mod` 5)
  mul (Mod5 a) (Mod5 b) = Mod5 ( (a * b)  `mod` 5)

instance Parsable Mod5 where
  parse x
    | Just (n, r) <- (parse x) = Just ((Mod5 n), r)
    | otherwise = Nothing

data Mat2x2 = Mat2x2 Integer Integer Integer Integer deriving (Show, Eq)

-- glad this is only a 2x2 matrix - more would be horrible
instance Ring Mat2x2 where
  add (Mat2x2 a11 a12 a21 a22) (Mat2x2 b11 b12 b21 b22) = (Mat2x2 s11 s12 s21 s22)
    where s11 = a11 + b11
          s12 = a12 + b12
          s21 = a21 + b21
          s22 = a22 + b22
  addId = (Mat2x2 0 0 0 0)
  addInv (Mat2x2 a b c d) = (Mat2x2 (negate a) (negate b) (negate c) (negate d))
  mul (Mat2x2 a11 a12 a21 a22) (Mat2x2 b11 b12 b21 b22) = (Mat2x2 p11 p12 p21 p22)
    where p11 = (a11 * b11) + (a12 * b21)
          p12 = (a11 * b12) + (a12 * b22)
          p21 = (a21 * b11) + (a22 * b21)
          p22 = (a21 * b12) + (a22 * b22)
  mulId = (Mat2x2 1 0 0 1)

instance Parsable Mat2x2 where
  parse x = do
    rest <- stripPrefix "[[" x
    (a, rest) <- parse rest
    rest <- stripPrefix "," rest
    (b, rest) <- parse rest
    rest <- stripPrefix "][" rest
    (c, rest) <- parse rest
    rest <- stripPrefix "," rest
    (d, rest) <- parse rest
    rest <- stripPrefix "]]" rest

    return ((Mat2x2 a b c d), rest)

instance Ring Bool where
  add = xor
  addId = False
  addInv x = False
  mul = (&&)
  mulId = True

instance Parsable Bool where
  parse str
    | Just rest <- stripPrefix "True" str = Just (True, rest)
    | Just rest <- stripPrefix "False" str = Just (False, rest)
    | otherwise = Nothing


xor :: Bool -> Bool -> Bool
xor True a = not a
xor False a = a