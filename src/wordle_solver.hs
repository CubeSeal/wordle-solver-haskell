{-# LANGUAGE DeriveDataTypeable #-}
import System.IO ()
import Data.Char ( toLower )
import Data.List ( group, nub, sort, sortBy, transpose, delete )
import Data.List.Split ( splitOn )
import Data.Data ( Data(toConstr) )
import Data.Maybe (catMaybes, mapMaybe, fromMaybe)
import Data.Function (on)
import Debug.Trace (trace)

-- Data types
-- Represent the coloured characters of the game.
data WordleChar = NoMatch {toChar :: Char}
    | Approx {toChar :: Char}
    | Exact {toChar :: Char}
    deriving (Read, Show, Eq, Data)

newtype ProbTable a = ProbTable {fromProbTable :: [(a, Double)]}
    deriving (Show, Eq)

type WordleResult = [WordleChar]
type WordList = [String]
type Mask = [Int]

-- Light Functions
toString :: WordleResult -> String
toString = map toChar

checkConst :: (Data g) => g -> g -> Bool
checkConst x y = toConstr x == toConstr y

dropExact :: WordleResult -> String -> String
dropExact (x:xs) (y:ys) | checkConst x (Exact y) = dropExact xs ys
                        | otherwise              = y : dropExact xs ys
dropExact _ _ = []

leftJoinProbTable :: Eq a => (Double -> Double -> Double) -> ProbTable a -> ProbTable a -> ProbTable a
leftJoinProbTable func leftTBL rightTBL = ProbTable $ zip index newProbs
    where
        index = map fst $ fromProbTable leftTBL
        leftProbs = map snd $ fromProbTable rightTBL
        rightProbs = map (\x -> fromMaybe 0 $ lookupTable x rightTBL) index
        newProbs = zipWith func leftProbs rightProbs

-- Main
main :: IO ()
main = do
    wordsRaw <- readFile "./res/words.txt"
    let wordList = words wordsRaw
    putStrLn "Enter word to solve:"
    finalWord <- getAndValInput
    print $ wordleSolver wordList (map toLower finalWord) []

-- Program Logic
getAndValInput :: IO String
getAndValInput = do
    unSafeInput <- getLine
    if length unSafeInput == 5
        then return unSafeInput
        else do
            putStrLn "Not a 5 letter word. Try again:"
            getAndValInput

wordleSolver :: WordList -> String -> [String] -> [String]
wordleSolver wordList finalWord accumGuess = if all (checkConst (Exact 'a')) guessFlag
    then accumGuess ++ [guess]
    else wordleSolver newWordList finalWord (accumGuess ++ [guess])
    where
        guess = trace (show guess2) guess2
        guess2 = scoreWords wordList
        guessFlag = trace (show guessFlag2) guessFlag2
        guessFlag2 = wordleGame guess finalWord
        newWordList = filter (`validWords` guessFlag) wordList

wordleGame :: [Char] -> [Char] -> WordleResult
wordleGame guess finalWord = zipWith func guess finalWord
    where
        func = \x y -> if x == y then Exact x else (if x `elem` finalWord then Approx x else NoMatch x)

scoreWords :: WordList -> String
scoreWords wordList = fst $ last sortedWordList
    where
        sortedWordList = sortBy (compare `on` snd) $ fromProbTable totalScore
        approxScore = ProbTable $ zip wordList $ map (`wordApproxScore` approxProbTable) wordList
        probScore = positionalScore wordList [0, 1]
        totalScore = leftJoinProbTable (+) approxScore probScore
        approxProbTable = generateProbTable $ concat wordList

lookupTable :: Eq a => a -> ProbTable a -> Maybe Double
lookupTable tok probTable = lookup tok $ fromProbTable probTable

wordApproxScore :: String -> ProbTable Char -> Double
wordApproxScore word probTable = sum $ mapMaybe (`lookupTable` probTable) $ nub word

positionalScore :: WordList -> Mask -> ProbTable String
positionalScore wordList mask = ProbTable o
    where
        splitWords = map (splitOn "_" . (\x -> replace x mask '_')) wordList
        splitCols = transpose splitWords
        colwiseFreqs = map tokProbs splitCols
        rowSums = map sum $ transpose colwiseFreqs
        o = zip wordList rowSums
        tokProbs :: [String] -> [Double]
        tokProbs tokList = map func tokList
            where
                func x = fromMaybe 0 $ lookupTable x probTable
                probTable = generateProbTable tokList

replace :: String -> [Int] -> Char -> String
replace xs i e = last $ iterate func i
    where
        iterate :: (a -> String) -> [a] -> [String]
        iterate _ [] = []
        iterate f (a:bc) = f a : iterate f bc
        func :: Int -> String
        func i = case splitAt i xs of
                        (before, _:after) -> before ++ e: after
                        _ -> xs

generateProbTable :: Ord a => [a] -> ProbTable a
generateProbTable ls = ProbTable output
    where
        output = map (\x -> (fst x, fromIntegral (snd x) / fromIntegral nLetters ) ) countTable
        nLetters = sum $ map snd countTable
        countTable = map (\x -> (head x, length x)) . group . sort $ ls

validWords :: String -> WordleResult -> Bool
validWords word guessFlag = and [wordCriteria, exactCriteria, approxCriteria, nomatchCriteria]
    where
        wordCriteria = word /= toString guessFlag
        exactCriteria = null ([ls | Exact ls <- guessFlag]) || or (zipWith (\x y -> Exact x == y) word guessFlag)
        approxCriteria = case [ls | Approx ls <- guessFlag] of
            [] -> True
            x -> any (`elem` x) word
        nomatchCriteria = not $ any (`elem` [ls | NoMatch ls <- guessFlag]) word