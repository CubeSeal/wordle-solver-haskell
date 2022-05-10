{-# LANGUAGE DeriveDataTypeable #-}
module App.Funcs where 

import Data.List ( delete, group, nub, sort, sortBy, transpose )
import Data.Data ( Data(toConstr) )
import Data.Maybe (mapMaybe, fromMaybe)
import Data.Function (on)

-- Data types
-- Represent the coloured characters of the game.
data WordleChar = NoMatch {fromWordleChar :: Char}
    | Approx {fromWordleChar :: Char}
    | Exact {fromWordleChar :: Char}
    deriving (Read, Show, Eq, Data)

newtype ProbTable a = ProbTable {fromProbTable :: [(a, Double)]}
    deriving (Show, Eq)

type WordleResult = [WordleChar]
type WordList = [String]
type Mask = [Int]

-- Light Functions
powerSet :: [a] -> [[a]]
powerSet [] = [[]]
powerSet (x:xs) = map (x:) (powerSet xs) ++ powerSet xs

toString :: WordleResult -> String
toString = map fromWordleChar

checkConst :: (Data g) => g -> g -> Bool
checkConst x y = toConstr x == toConstr y

dropApprox :: WordleResult -> String -> String
dropApprox (x:xs) (y:ys)
    | checkConst x (Approx y) = dropApprox xs ys
    | otherwise               = y : dropApprox xs ys
dropApprox _ _ = []

lookupTable :: Eq a => a -> ProbTable a -> Maybe Double
lookupTable tok probTable = lookup tok $ fromProbTable probTable

replace :: String -> [Int] -> Char -> String
replace ls [] _ = ls
replace ls (i:is) e = replace (func i ls) is e
    where
        func :: Int -> String -> String
        func i' xs = case splitAt i' xs of
                        (before, _:after) -> before ++ e: after
                        _ -> xs
-- Wordle Functions
wordleGame :: [Char] -> [Char] -> WordleResult
wordleGame guess finalWord = zipWith func guess finalWord
    where
        func = \x y -> if x == y then Exact x else (if x `elem` finalWord then Approx x else NoMatch x)

scoreWords :: WordList -> String
scoreWords wordList = fst $ last sortedWordList
    where
        sortedWordList = sortBy (compare `on` snd) totalScore
        totalScore = zip wordList $ zipWith (+) approxScore probScore
        -- Approximate score
        approxScore = map (`wordApproxScore` approxProbTable) wordList
        approxProbTable = generateProbTable $ concat wordList
        -- Positional score
        probScore = map sum $ transpose probScores
        probScores = map (posScore wordList) allMasks
        allMasks = delete [] $ powerSet [0,1.. 4]

wordApproxScore :: String -> ProbTable Char -> Double
wordApproxScore word probTable = sum $ mapMaybe (`lookupTable` probTable) $ nub word

posScore :: WordList -> Mask -> [Double]
posScore wordList mask = rowSums
    where
        rowSums = map sum $ transpose colwiseFreqs
        colwiseFreqs = map tokProbs splitCols
        splitCols = transpose splitWords
        splitWords = map (words . (\x -> replace x mask ' ')) wordList
        tokProbs :: [String] -> [Double]
        tokProbs tokList = map func tokList
            where
                func x = fromMaybe 0 $ lookupTable x probTable
                probTable = generateProbTable tokList

generateProbTable :: Ord a => [a] -> ProbTable a
generateProbTable ls = ProbTable output
    where
        output = map func countTable
        func (x, y) = (x, prob y * (1 - prob y))
        prob x = fromIntegral x / fromIntegral nLetters
        nLetters = sum $ map snd countTable
        countTable = map (\x -> (head x, length x)) . group . sort $ ls

validWords :: String -> WordleResult -> Bool
validWords word guessFlag = and [wordCriteria, exactCriteria, approxCriteria, nomatchCriteria]
    where
        wordCriteria = word /= toString guessFlag
        exactCriteria = null ([ls | Exact ls <- guessFlag])
            || and (zipWith (\x y -> case y of {Exact b -> x == b; _ -> True}) word guessFlag)
        approxCriteria = case [ls | Approx ls <- guessFlag] of
            [] -> True
            approxMatches -> any (`elem` approxMatches) (dropApprox guessFlag word)
                && and (zipWith (\x y -> Approx x /= y) word guessFlag)
        nomatchCriteria = not $ any (`elem` [ls | NoMatch ls <- guessFlag]) word