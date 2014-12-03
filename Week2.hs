{-
Name: <your name here>
Collaborators: <your collaborators here, or "none">
Notes: <any particular notes about your work -- what you struggled with,
        what's not working, what's really cool, etc.>
-}

module Week2 where

import Words
import Data.List
import Data.List.Split
import Text.Regex.Posix


-- Though a Scrabble hand is the same Haskell type as a Scrabble word, they
-- have different properties. Specifically, a hand is unordered whereas a word
-- is ordered. We denote this distinction by using a type synonym to talk
-- about hands, even though we could just say `String`.
type Hand = [Char]

-- A `Template` is like a word, but it has '?' characters in some places as
-- placeholders for letters from a player's hand. Because real words do not
-- have '?' characters, we use another type synonym to track this distinction.
type Template = String

-- A 'STemplate' is like a template, but it has markers to indicate four kinds
-- of special board locations: double-letter noted with 'D', triple-letter
-- noted with 'T', double-word noted with '2', and triple-word noted with '3'.
-- For matching, these behave just like '?' does -- they can be filled in with
-- any letter. But, when scoring, any letter played on a 'D' gets double its
-- value, and any letter played on a 'T' gets triple its value. If any square
-- in the template is a '2', the whole word's value is doubled; if any square
-- in the template is a '3', the whole word's score is tripled. If multiple of
-- these special squares are in the same word, the effects multiply.
type STemplate = Template

-- Write your code below:
formableBy :: String -> Hand -> Bool
formableBy [] _ = True
formableBy _ [] = False
formableBy (x:xs) b
    | elem x b == False = False
    | otherwise = formableBy xs (delete x b)
 

wordsFrom :: Hand -> [String]
wordsFrom hand = filter (`formableBy` hand) allWords

wordFitsTemplate :: Template -> Hand -> String -> Bool
wordFitsTemplate t h s 
    | length t /= length s = False 
    | t == s = True
    | otherwise = formableBy s (buildTiles t h)


wordsFittingTemplate :: Template -> Hand -> [String]
wordsFittingTemplate t h = wordsFrom (buildTiles t h) 

scrabbleValueWord :: String -> Int
scrabbleValueWord s = undefined

bestWords :: [String] -> [String]
bestWords s = undefined

scrabbleValueTemplate :: STemplate -> String -> Int
scrabbleValueTemplate = undefined

buildTiles :: [Char] -> [Char] -> [Char]
buildTiles t h = concat (filter (\x -> x /= "") (splitOn "?" t)) ++ h

matchesPattern :: String -> String -> Bool
matchesPattern t w 
       | length t /= length w = False
       | otherwise = True 