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
import Data.Foldable
import Data.Maybe (fromMaybe, mapMaybe)
import Control.Monad (filterM)
import qualified Data.List.NonEmpty as N

-- Data Types --

-- Represent the coloured letters of the game
data WordleChar
  = NoMatch {fromWordleChar :: Char}
  | Approx {fromWordleChar :: Char}
  | Exact {fromWordleChar :: Char}
  deriving (Read, Show, Eq, Data)

type WordleString = N.NonEmpty WordleChar -- String of WordleChar's

-- Represent probability tables (analogous to frequency tables)
newtype ScoreTable a = ScoreTable {fromScoreTable :: N.NonEmpty (a, Double)}
  deriving (Show, Eq)
-- Creates score table from list
createScoreTable :: Ord a => N.NonEmpty a -> ScoreTable a
createScoreTable ls = ScoreTable $ N.map func freqTable
  where
    func (x, y) = (x, prob y * (1 - prob y))
    prob x = fromIntegral x / fromIntegral lsLength
    lsLength = N.length ls
    freqTable = N.map (\x -> (N.head x, N.length x)) . N.group1 . N.sort $ ls

lookupTable :: Eq a => a -> ScoreTable a -> Maybe Double
lookupTable x (ScoreTable y) = lookup x $ N.toList y

-- Simple types
type NString = N.NonEmpty Char
type WordList = N.NonEmpty NString
type Mask = N.NonEmpty Int -- Used for generating tokens

-- Helper Functions -- 
cconcat :: N.NonEmpty (N.NonEmpty a) -> N.NonEmpty a
cconcat xs = foldr1 (\x y -> ) b (t a)

powerSet :: [a] -> [[a]]
powerSet = filterM $ const [True, False]

toString :: WordleString -> String
toString = N.toList . N.map fromWordleChar

toWordleString :: String -> String -> Maybe WordleString
toWordleString x y = do
  a <- N.nonEmpty x
  b <- N.nonEmpty y
  return $ N.zipWith func a b
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

-- Wordle Functions -- 

-- Plays wordle with known secret answer
wordleGame :: NString -> NString -> WordleString
wordleGame guess answer = N.zipWith func guess answer
  where
    func = \x y -> if x == y
      then Exact x
      else if x `elem` answer
        then Approx x
        else NoMatch x

-- Gets the 'best' word out of a list of words
scoreWords :: WordList -> NString
scoreWords wl = fst $ last sortedWordList
  where
    sortedWordList = sortBy (compare `on` snd) totalScore
    totalScore = N.zip wl $ N.zipWith (+) approxScore probScore
    -- Approximate score
    approxScore = N.map (`wordApproxScore` approxScoreTable) wl
    approxScoreTable = createScoreTable $ concat wl
    -- Positional score
    probScore = map sum $ transpose probScores
    probScores = map (posScores wordList) allMasks
    allMasks = N.fromList $ delete [] $ powerSet ([0, 1.. 4] :: [Int])

-- Generates the approximate score for a word given a ScoreTable Char
wordApproxScore :: NString -> ScoreTable Char -> Double
wordApproxScore word scoreTable = sum $ mapMaybe (`lookupTable` scoreTable) $ N.toList $ N.nub word

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