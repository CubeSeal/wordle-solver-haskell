{-# LANGUAGE DeriveDataTypeable #-}
import System.IO ()
import Data.Char ( toLower )
import Data.List ( group, nub, sort, sortBy, transpose )
import Data.List.Split ( splitOn )
import Data.Data ( Data(toConstr) )
import Data.Maybe (catMaybes, mapMaybe, fromMaybe)
import Data.Function (on)
import Debug.Trace (trace)

-- Represent the coloured characters of the game.
data WordleChar = NoMatch {toChar:: Char} | Approx {toChar:: Char} | Exact {toChar:: Char}
    deriving (Read, Show, Eq, Data)
newtype ProbTable a = ProbTable [([a], Double)]
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

-- Main
main :: IO()
main = do
    wordsRaw <- readFile "./res/words.txt"
    let wordList = words wordsRaw
    putStrLn "Enter word to solve:"
    finalWord <- getLine
    print $ wordleSolver wordList (map toLower finalWord) []

-- Program Logic Functions
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
        sortedWordList = sortBy (compare `on` snd) approxScore
        approxScore = zip wordList $ map (`wordApproxScore` probTable) wordList
        probTable = generateProbTable $ concat wordList

wordApproxScore :: String -> ProbTable Char -> Double
wordApproxScore word probTable = sum $ mapMaybe lookupTable $ nub word
    where
        lookupTable x = lookup [x] $ toRaw probTable
        toRaw (ProbTable x) = x

-- positionalScore :: WordList -> Mask -> FreqTable String
-- positionalScore wordList mask =
--     where
--         splitWords = map (splitOn "_" . (\x -> replace x mask '_')) wordList
--         splitCols = transpose splitWords
--             where
--                 k = map lookupFreq ls
--                 lookupFreq x = lookup [x] $ toRaw freqTable
--                 toRaw (Table x) = x
--                 freqTable = generateFreqTable ls


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
        countTable = map (\x -> ([head x], length x)) . group . sort $ ls

validWords :: String -> WordleResult -> Bool
validWords word guessFlag = and [wordCriteria, exactCriteria, approxCriteria, nomatchCriteria]
    where
        wordCriteria = word /= toString guessFlag
        exactCriteria = null ([ls | Exact ls <- guessFlag]) || or (zipWith (\x y -> Exact x == y) word guessFlag)
        approxCriteria = case [ls | Approx ls <- guessFlag] of
            [] -> True
            x -> any (`elem` x) $ dropExact guessFlag word
        nomatchCriteria = not $ any (`elem` [ls | NoMatch ls <- guessFlag]) word