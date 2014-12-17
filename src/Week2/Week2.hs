{-
Name: <your name here>
Collaborators: <your collaborators here, or "none">
Notes: <any particular notes about your work -- what you struggled with,
        what's not working, what's really cool, etc.>
-}

module Week2.Week2 where

import Week2.Words
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

-- not same length then bail out
-- if same then also know
-- otherwise we need to check
wordFitsTemplate :: Template -> Hand -> String -> Bool
wordFitsTemplate t h s 
    | length t /= length s = False 
    | t == s = True
    | otherwise = formableBy s (buildTiles t h)


-- list comprehension to 
wordsFittingTemplate :: Template -> Hand -> [String]
wordsFittingTemplate t h = [x | x <- (wordsFrom (buildTiles t h)),(matchesPattern t x)]

scrabbleValueWord :: String -> Int
scrabbleValueWord s = sum $ map (scrabbleValue) s

bestWords :: [String] -> [String]
bestWords s = [x | x <- s, ((scrabbleValueWord x) ==  (maximum $ map scrabbleValueWord s))]

-- uses helper functions to calculate the score...
scrabbleValueTemplate :: STemplate -> String -> Int
scrabbleValueTemplate st word = applyWordMulti st (applyLetterMulti st word)

buildTiles :: [Char] -> [Char] -> [Char]
buildTiles t h = concat (filter (\x -> x /= "") (splitOn "?" t)) ++ h

-- determines if it matches by zipping the template and the hand together with the func matchelem and folding.  If it's all True it stays True but if anything fails it is False
matchesPattern :: Template -> String -> Bool
matchesPattern t w 
       | length t /= length w = False
       | t == w = True
       | otherwise = foldl (&&) True (zipWith matchElem t w) 

-- build a list of True/False so we can zip it
matchElem :: Char -> Char -> Bool
matchElem templateChar wordChar 
      | templateChar == wordChar = True
      | templateChar == '?' = True
      | otherwise = False

applyLetterMulti :: STemplate -> String -> Int
applyLetterMulti st word = sum $ zipWith stElem st word

applyWordMulti :: STemplate -> Int -> Int
applyWordMulti st ewv 
      | elem 'D' st = ewv * 2
      | elem 'T' st = ewv * 3
      | elem 'D' st  && elem 'T' st = ewv * 6
      | otherwise = ewv

stElem :: Char -> Char -> Int
stElem ste we 
      | ste == '2' = scrabbleValue we * 2
      | ste == '3' = scrabbleValue we * 3
      | otherwise = scrabbleValue we 

-- Bonus
matchesPattern2 :: Template -> String -> Bool
matchesPattern2 t w 
       | length t < length w = False
       | t == w = True
       | otherwise = foldl (&&) True (zipWith matchElem t w) 
