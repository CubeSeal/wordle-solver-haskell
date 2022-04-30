{-# LANGUAGE DeriveDataTypeable #-}
import System.IO ()
import Data.Char ( toLower )
import Data.List
import Data.Data ( Data(toConstr) )
import Data.Maybe (catMaybes, mapMaybe)
import Data.Function (on)
import Debug.Trace (trace)

data WordleChar = NoMatch Char | Approx Char | Exact Char
    deriving (Read, Show, Eq, Data)

type Table = [([Char], Double)]
type WordleResult = [WordleChar]
type WordList = [String]

toString :: WordleResult -> String
toString = map toChar

toChar :: WordleChar -> Char
toChar (NoMatch x) = x
toChar (Approx x) = x
toChar (Exact x) = x

checkConst :: (Data g) => g -> g -> Bool
checkConst x y = toConstr x == toConstr y

main :: IO()
main = do
    words_raw <- readFile "./res/words.txt"
    let word_list = words words_raw
    putStrLn "Enter word to solve:"
    final_word <- getLine
    print $ wordleSolver word_list (map toLower final_word) []

wordleSolver :: WordList -> String -> [String] -> [String]
wordleSolver word_list final_word accum_guess = if all (checkConst (Exact 'a')) guess_flag
    then accum_guess ++ [guess]
    else wordleSolver new_word_list final_word (accum_guess ++ [guess])
    where
        guess = scoreWords word_list
        guess_flag = wordleGame guess final_word
        new_word_list = filter (`validWords` guess_flag) word_list

wordleGame :: [Char] -> [Char] -> WordleResult
wordleGame guess final_word = zipWith func guess final_word
    where
        func = \x y -> if x == y then Exact x else (if x `elem` final_word then Approx x else NoMatch x)

scoreWords :: WordList -> String
scoreWords word_list = fst $ last sorted_word_list
    where
        sorted_word_list = sortBy (compare `on` snd) approx_score
        approx_score = zip word_list $ map (`approxScore` freq_table) word_list
        freq_table = freqTable $ concat word_list

approxScore :: String -> Table -> Double
approxScore word freq_table = sum $ mapMaybe (\x -> lookup [x] freq_table) $ nub word

freqTable :: [Char] -> Table
freqTable ls = map (\x -> (fst x, fromIntegral (snd x) / fromIntegral n_letters ) ) count_table
    where
        n_letters = sum $ map snd count_table
        count_table = map (\x -> ([head x], length x)) . group . sort $ ls

validWords :: String -> WordleResult -> Bool
validWords word guess_flag = x && y && z && w
    where
        w = word /= toString guess_flag
        x = if length [ls | Exact ls <- guess_flag] == 0
                then True
                else or $ zipWith (\x y -> Exact x == y) word guess_flag
        y = if length [ls | Approx ls <- guess_flag] == 0
                then True
                else any (`elem` [ls | Approx ls <- guess_flag]) word
        z = not $ any (`elem` [ls | NoMatch ls <- guess_flag]) word