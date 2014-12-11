{-# OPTIONS_GHC -Wall #-}

module Week3 where

import Log
import Data.List
import Data.Char


parseMessage :: String -> MaybeLogMessage
parseMessage message = case (words message !! 0) of
                            "I" -> ValidLM $ LogMessage Info (toInt (words message !! 1)) (unwords $ drop 2 $ words message)
                            "E" -> ValidLM $ LogMessage (Error (toInt (words message !! 1))) (toInt (words message !! 2)) (unwords $ drop 3 $ words message)
                            _  -> InvalidLM message

toInt :: String -> Int
toInt s = read s :: Int

validMessagesOnly :: [MaybeLogMessage] -> [LogMessage]
validMessagesOnly mlm = [x | ValidLM x <- mlm]

parse :: String -> [LogMessage]
parse s = validMessagesOnly $ map (\x-> parseMessage x) (lines s)

compareMsgs :: LogMessage -> LogMessage -> Ordering
compareMsgs (LogMessage a b c) (LogMessage d e f) 
       | b == e = EQ
       | b < e = LT
       | otherwise = GT

sortMessages :: [LogMessage] -> [LogMessage]
sortMessages llm = sortBy compareMsgs llm

whatWentWrong :: [LogMessage] -> [String]
whatWentWrong lms = [str | (LogMessage _ _ str) <- badErrors lms]

badErrors :: [LogMessage] -> [LogMessage]
badErrors mlm = sortMessages $ [x | x@(LogMessage (Error (s)) _ _) <- mlm, s > 50]

messagesAbout :: String -> [LogMessage] -> [LogMessage]
messagesAbout s lm = [x | x@(LogMessage _ _ em) <- lm, isInfixOf (tL s) (tL em)]

whatWentWrongEnhanced :: String -> [LogMessage] -> [String]
whatWentWrongEnhanced s lm = undefined

(|||) :: (LogMessage -> Bool) -> (LogMessage -> Bool) -> LogMessage -> Bool
(|||) f g x = f x || g x -- (||) is Haskell’s ordinary "or" operator

tL :: String -> String
tL s = map toLower s