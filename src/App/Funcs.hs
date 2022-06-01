{-# LANGUAGE DeriveDataTypeable #-}

module App.Funcs
  ( WordleString,
    ScoreTable,
    WordList,
    toWordleString,
    isAllExact,
    wordleGame,
    scoreWords,
    validWords,
  )
where

-- External Modules
import Data.Data (Data (toConstr))
import Data.Function (on)
import Data.List (delete, group, nub, sort, sortBy, transpose, foldl')
import Data.Maybe (fromMaybe, mapMaybe)
import Control.Monad (filterM)

-- Data Types

-- Represent the coloured letters of the game
data WordleChar
  = NoMatch {fromWordleChar :: Char}
  | Approx {fromWordleChar :: Char}
  | Exact {fromWordleChar :: Char}
  deriving (Read, Show, Eq, Data)

-- Represent probability tables (analogous to frequency tables)
newtype ScoreTable a = ScoreTable {fromScoreTable :: [(a, Double)]}
  deriving (Show, Eq)

type WordleString = [WordleChar] -- String of WordleChar's
type WordList = [String]
type Mask = [Int] -- Used for generating tokens

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

lookupTable :: Eq a => a -> ScoreTable a -> Maybe Double
lookupTable tok scoreTable = lookup tok $ fromScoreTable scoreTable

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
scoreWords wordList = fst $ last sortedWordList
  where
    sortedWordList = sortBy (compare `on` snd) totalScore
    totalScore = zip wordList $ zipWith (+) approxScore probScore
    -- Approximate score
    approxScore = map (`wordApproxScore` approxScoreTable) wordList
    approxScoreTable = createScoreTable $ concat wordList
    -- Positional score
    probScore = map sum $ transpose probScores
    probScores = map (posScores wordList) allMasks
    allMasks = delete [] $ powerSet [0, 1.. 4]

-- Generates the approximate score for a word given a ScoreTable Char
wordApproxScore :: String -> ScoreTable Char -> Double
wordApproxScore word scoreTable = sum $ mapMaybe (`lookupTable` scoreTable) $ nub word

-- Generates the positional scores for a list of words given a Mask
posScores :: WordList -> Mask -> [Double]
posScores wordList mask = rowSums
  where
    rowSums = map sum $ transpose colwiseFreqs
    colwiseFreqs = map tokProbs splitCols
    splitCols = transpose splitWords
    splitWords = map (words . (\x -> replace x mask ' ')) wordList
    tokProbs :: [String] -> [Double] -- Generates token scores
    tokProbs tokList = map func tokList
      where
        func x = fromMaybe 0 $ lookupTable x scoreTable
        scoreTable = createScoreTable tokList

-- Creates score table from list
createScoreTable :: Ord a => [a] -> ScoreTable a
createScoreTable ls = ScoreTable $ map func freqTable
  where
    func (x, y) = (x, prob y * (1 - prob y))
    prob x = fromIntegral x / fromIntegral lsLength
    lsLength = length ls
    freqTable = map (\x -> (head x, length x)) . group . sort $ ls

-- Checks if word is a valid guess given WordleString information
validWords :: String -> WordleString -> Bool
validWords word guessFlag =
  and
    [ wordCriteria,
      exactCriteria,
      approxCriteria,
      nomatchCriteria
    ]
  where
    wordCriteria = word /= toString guessFlag
    exactCriteria = case [ls | Exact ls <- guessFlag] of
      [] -> True
      _  -> and (zipWith (\x y -> case y of Exact b -> x == b; _ -> True) word guessFlag)
    approxCriteria = case [ls | Approx ls <- guessFlag] of
      []            -> True
      approxMatches -> any (`elem` approxMatches) word
        && and (zipWith (\x y -> Approx x /= y) word guessFlag)
    nomatchCriteria = not $ any (`elem` [ls | NoMatch ls <- guessFlag]) word