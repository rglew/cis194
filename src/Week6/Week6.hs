{-# LANGUAGE DeriveGeneric, OverloadedStrings #-}

module Week6.Week6 where

import Data.Aeson
import Data.Monoid
import Control.Applicative
import GHC.Generics
import qualified Data.ByteString.Lazy.Char8 as B
import qualified Data.Text as T
import qualified Data.Text.IO as T


data Market = Market { fmid :: Integer
                     , marketname :: T.Text
                     , website :: T.Text
                     , street :: T.Text
                     , city :: T.Text
                     , county :: T.Text
                     , state :: T.Text
                     , zip :: T.Text
                     , season1date :: T.Text
                     , season1time :: T.Text
                     , season2date :: T.Text
                     , season2time :: T.Text
                     , season3date :: T.Text
                     , season3time :: T.Text
                     , season4date :: T.Text
                     , season4time :: T.Text
                     , x :: Double
                     , y :: Double
                     , location :: T.Text
                     , credit :: Bool
                     , wic :: Bool
                     , wiccash :: Bool
                     , sfmnp :: Bool
                     , snap :: Bool
                     , bakedgoods :: Bool
                     , cheese :: Bool
                     , crafts :: Bool
                     , flowers :: Bool
                     , eggs :: Bool
                     , seafood :: Bool
                     , herbs :: Bool
                     , vegetables :: Bool
                     , honey :: Bool
                     , jams :: Bool
                     , maple :: Bool
                     , meat :: Bool
                     , nursery :: Bool
                     , nuts :: Bool
                     , plants :: Bool
                     , poultry :: Bool
                     , prepared :: Bool
                     , soap :: Bool
                     , trees :: Bool
                     , wine :: Bool
                     } deriving (Show, Generic)

instance FromJSON Market
  

-- ex1
ynToBool :: Value -> Value
ynToBool (String "Y") = (Bool True)
ynToBool (String "N") = (Bool False)
ynToBool (Object o) = Object $ fmap ynToBool o
ynToBool (Array a)  = Array  $ fmap ynToBool a
ynToBool v = v

-- ex2
parseData :: B.ByteString -> Either String Value
parseData d = fmap ynToBool $ eitherDecode d -- still don't understand why eitherDecode d and decode d fail when just applied to the bytestrean in ghci

getJSON :: IO B.ByteString
getJSON = B.readFile "Week6\\markets.json"

result2Either :: Result a -> Either String a
result2Either (Success s) = Right s
result2Either (Error e) = Left e

parseMarkets :: B.ByteString -> Either String [Market]
parseMarkets bytes = (parseData bytes) >>= (result2Either . fromJSON)

loadData :: IO [Market]
loadData = do
  filedata <- getJSON
  let marketData = parseMarkets $ filedata
  case marketData of
   Left err -> fail err
   Right markets -> return markets