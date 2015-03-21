{-# LANGUAGE DeriveGeneric, OverloadedStrings #-}

module Week6.Week6 where

import Data.Aeson
import Data.Monoid
import GHC.Generics
import qualified Data.ByteString.Lazy.Char8 as B
import qualified Data.Text as T
import qualified Data.Text.IO as T

ynToBool :: Value -> Value
ynToBool v  
        | v == String "Y" = (Bool True)
        | v == String "N" = (Bool False)
        | otherwise = v