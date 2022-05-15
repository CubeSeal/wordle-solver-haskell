import App.Funcs
  ( WordList,
    isAllExact,
    scoreWords,
    validWords,
    wordleGame,
  )
import Data.Char (toLower)

-- Main
main :: IO ()
main = do
  wordsRaw <- readFile "./res/words.txt"
  let wordList = words wordsRaw
  putStrLn "Enter word to solve:"
  finalWord <- getAndValInput
  print $ wordleSolver wordList (map toLower finalWord) []

-- Make sure the user isn't fucking with me
getAndValInput :: IO String
getAndValInput = do
  unSafeInput <- getLine
  if length unSafeInput == 5
    then return unSafeInput
    else do
      putStrLn "Not a 5 letter word. Try again:"
      getAndValInput

-- Program Logic
wordleSolver :: WordList -> String -> [String] -> [String]
wordleSolver wordList finalWord accumGuess =
  if isAllExact guessFlag
    then accumGuess ++ [guess]
    else wordleSolver newWordList finalWord (accumGuess ++ [guess])
  where
    newWordList = filter (`validWords` guessFlag) wordList
    guessFlag = wordleGame guess finalWord
    guess = scoreWords wordList