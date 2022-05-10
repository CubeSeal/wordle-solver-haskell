{-# LANGUAGE DeriveDataTypeable #-}
import System.IO ()
import Data.Char ( toLower )
import Data.List ( delete, group, nub, sort, sortBy, transpose )
import Data.List.Split ( wordsBy )
import Data.Data ( Data(toConstr) )
import Data.Maybe (mapMaybe, fromMaybe)
import Data.Function (on)

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
powerSet :: [a] -> [[a]]
powerSet [] = [[]]
powerSet (x:xs) = map (x:) (powerSet xs) ++ powerSet xs

toString :: WordleResult -> String
toString = map toChar

checkConst :: (Data g) => g -> g -> Bool
checkConst x y = toConstr x == toConstr y

leftJoinProbTable :: Eq a => (Double -> Double -> Double) -> ProbTable a -> ProbTable a -> ProbTable a
leftJoinProbTable func leftTBL rightTBL = ProbTable $ zip index newProbs
    where
        index = map fst $ fromProbTable leftTBL
        leftProbs = map snd $ fromProbTable leftTBL
        rightProbs = map (\x -> fromMaybe 0 $ lookupTable x rightTBL) index
        newProbs = zipWith func leftProbs rightProbs

dropApprox :: WordleResult -> String -> String
dropApprox (x:xs) (y:ys)
    | checkConst x (Approx y) = dropApprox xs ys
    | otherwise               = y : dropApprox xs ys
dropApprox _ _ = []

lookupTable :: Eq a => a -> ProbTable a -> Maybe Double
lookupTable tok probTable = lookup tok $ fromProbTable probTable

replace :: String -> [Int] -> Char -> String
replace xs i e = last $ iterate' func i
    where
        iterate' :: (a -> String) -> [a] -> [String]
        iterate' _ [] = []
        iterate' f (a:bc) = f a : iterate' f bc
        func :: Int -> String
        func i' = case splitAt i' xs of
                        (before, _:after) -> before ++ e: after
                        _ -> xs

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
        guess = scoreWords wordList
        guessFlag = wordleGame guess finalWord
        newWordList = filter (`validWords` guessFlag) wordList

wordleGame :: [Char] -> [Char] -> WordleResult
wordleGame guess finalWord = zipWith func guess finalWord
    where
        func = \x y -> if x == y then Exact x else (if x `elem` finalWord then Approx x else NoMatch x)

scoreWords :: WordList -> String
scoreWords wordList = fst $ last sortedWordList
    where
        sortedWordList = sortBy (compare `on` snd) $ fromProbTable totalScore
        totalScore = leftJoinProbTable (+) approxScore probScore
        -- Approximate score
        approxScore = ProbTable $ zip wordList $ map (`wordApproxScore` approxProbTable) wordList
        approxProbTable = generateProbTable $ concat wordList
        -- Positional score
        probScores = map (positionalScore wordList) allMasks
        probScore = ProbTable $ zip wordList $ map sum $ transpose probScores
        allMasks = delete [] $ powerSet [0,1.. 4]

wordApproxScore :: String -> ProbTable Char -> Double
wordApproxScore word probTable = sum $ mapMaybe (`lookupTable` probTable) $ nub word

positionalScore :: WordList -> Mask -> [Double]
positionalScore wordList mask = rowSums
    where
        rowSums = map sum $ transpose colwiseFreqs
        colwiseFreqs = map tokProbs splitCols
        splitCols = transpose splitWords
        splitWords = map (wordsBy (== '_') . (\x -> replace x mask '_')) wordList
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
        exactCriteria = null ([ls | Exact ls <- guessFlag]) || and (zipWith (\x y -> case y of {Exact b -> x == b; _ -> True}) word guessFlag)
        approxCriteria = case [ls | Approx ls <- guessFlag] of
            [] -> True
            approxMatches -> any (`elem` approxMatches) (dropApprox guessFlag word) && and (zipWith (\x y -> Approx x /= y) word guessFlag)
        nomatchCriteria = not $ any (`elem` [ls | NoMatch ls <- guessFlag]) word