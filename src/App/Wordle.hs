{-# LANGUAGE DeriveDataTypeable #-}

module App.Wordle
  ( WordleString,
    WordList,
    toWordleString,
    isAllExact,
    wordleGame,
    scoreWords,
    isValidWord,
  )
where

-- External Modules

import App.ScoreTable ( ScoreTable, lookupTable, createTable, zipTable, maxTable )
import Data.Data (Data (toConstr))
import Data.List (delete, nub, transpose, foldl')
import Data.Maybe (mapMaybe, fromMaybe)
import Control.Monad (filterM)

-- Data Types

-- Represent the coloured letters of the game
data WordleChar
  = NoMatch {fromWordleChar :: Char}
  | Approx {fromWordleChar :: Char}
  | Exact {fromWordleChar :: Char}
  deriving (Read, Show, Eq, Data)

type WordleString = [WordleChar] -- String of WordleChar's

type WordList = [String]

-- Helper Functions
powerSet :: [a] -> [[a]]
powerSet = filterM $ const [True, False]

toString :: WordleString -> String
toString = map fromWordleChar

toWordleString :: String -> String -> WordleString
toWordleString = zipWith func
  where
    func a 'e' = Exact a
    func a 'a' = Approx a
    func a 'n' = NoMatch a
    func a _   = NoMatch a

checkConst :: (Data g) => g -> g -> Bool
checkConst x y = toConstr x == toConstr y

isAllExact :: WordleString -> Bool
isAllExact = all (checkConst (Exact 'a'))

-- Replaces elements of string with char with given indices.
replace :: String -> [Int] -> Char -> String
replace string [] _ = string
replace string indices char = foldl' (\x y -> replace' x y char) string indices
  where
    replace' :: String -> Int -> Char -> String
    replace' str i' e = case splitAt i' str of
      (before, _ : after) -> before ++ e : after
      _                   -> str

-- Wordle Functions

-- Plays wordle with known secret answer
wordleGame :: [Char] -> [Char] -> WordleString
wordleGame guess answer = zipWith func guess answer
  where
    func = \x y -> if x == y
      then Exact x
      else if x `elem` answer
        then Approx x
        else NoMatch x

-- Gets the 'best' word out of a list of words
scoreWords :: WordList -> String
scoreWords wl = fst $ maxTable s
  where
    s = a <> p
    -- Approximate score
    a = approxScore wl
    -- Positional score
    p = posScore wl

approxScore :: WordList -> ScoreTable String
approxScore wl = zipTable wl $ map f wl
  where
    f w = sum $ mapMaybe (`lookupTable` tbl) $ nub w
    tbl = createTable $ concat wl

posScore :: WordList -> ScoreTable String
posScore wl = zipTable wl scores
  where
    toks = map tokeniseWord wl
    colsOfToks = transpose toks
    colwiseScores = map createTBLAndGetVals colsOfToks
    scores = map sum $ transpose colwiseScores
    createTBLAndGetVals :: [String] -> [Double]
    createTBLAndGetVals t = map func t
      where
        func x = fromMaybe 0 $ lookupTable x tbl
        tbl = createTable t

tokeniseWord :: String -> [String]
tokeniseWord s = concatMap f mask
  where
    f i = words $ replace s i ' '
    mask = delete [] $ powerSet [0, 1..4]

isValidWord :: String -> WordleString -> Bool
isValidWord w g = all (\f -> f w g) listOfFuncs
  where
    listOfFuncs = [wordCriteria, exactCriteria, approxCriteria, noMatchCriteria]

wordCriteria :: String -> WordleString -> Bool
wordCriteria w g = w /= toString g

exactCriteria :: String -> WordleString -> Bool
exactCriteria w g = case [ls | Exact ls <- g] of
  [] -> True
  -- If all Exact WordleChars match the associated Char of the string.
  _  -> and $ zipWith doExactsMatch w g
  where
    doExactsMatch w' (Exact g') = w'== g'
    doExactsMatch _ _ = True

approxCriteria :: String -> WordleString -> Bool
approxCriteria w g = case [ls | Approx ls <- g] of
  [] -> True
  {- If Approx WordleChars are in word AND if they do not match the associated Char of the
  String -}
  ls  -> all (`elem` w) ls && and (zipWith approxNotInPos w g)
  where
    approxNotInPos w' g' = Approx w' /= g'

noMatchCriteria :: String -> WordleString -> Bool
noMatchCriteria w g = not $ any (`elem` [ls | NoMatch ls <- g ]) w