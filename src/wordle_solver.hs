{-# LANGUAGE DeriveDataTypeable #-}
import System.IO ()
import Data.Char ( toLower )
import Data.Data ( Data(toConstr) )

data WordleChar = NoMatch Char | Approx Char | Exact Char
    deriving (Read, Show, Eq, Data)

type WordleResult = [WordleChar]

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

wordleSolver :: [String] -> String -> [String] -> [String]
wordleSolver word_list final_word guess_list = if all (checkConst (Exact 'a')) guess_flag
    then guess_list ++ [guess]
    else wordleSolver new_word_list final_word (guess_list ++ [guess])
    where
        guess = scoreWords word_list
        guess_flag = wordleGame guess final_word
        new_word_list = filter (`validWords` guess_flag) word_list

wordleGame :: [Char] -> [Char] -> WordleResult
wordleGame guess final_word = zipWith (\x y -> if x == y then Exact x else (if elem x final_word then Approx x else NoMatch x)) guess final_word

scoreWords :: [String] -> String
scoreWords = head

validWords :: String -> WordleResult -> Bool
validWords word guess_flag = (x || y) && z && w
    where
        w = word /= toString guess_flag
        x = or $ zipWith (==) word $ toString guess_flag
        y = any (`elem` [x | Approx x <- guess_flag]) word
        z = not $ any (`elem` [x | NoMatch x <- guess_flag]) word