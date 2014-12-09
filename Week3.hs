{-# OPTIONS_GHC -Wall #-}

module Week3 where

import Log

parseMessage :: String -> MaybeLogMessage
parseMessage message = case (words message !! 0) of
                            "I" -> ValidLM $ LogMessage Info (toInt (words message !! 1)) (unwords $ drop 2 $ words message)
                            "E" -> ValidLM $ LogMessage (Error (toInt (words message !! 1))) (toInt (words message !! 2)) (unwords $ drop 3 $ words message)
                            _  -> InvalidLM message

toInt :: String -> Int
toInt s = read s :: Int