import Data.Char ( toLower )
import App.Funcs

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
        newWordList = filter (`validWords` guessFlag) wordList
        guessFlag = wordleGame guess finalWord
        guess = scoreWords wordList