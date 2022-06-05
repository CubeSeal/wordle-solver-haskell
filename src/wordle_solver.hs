import App.Funcs
  ( WordList,
    toWordleString,
    isAllExact,
    scoreWords,
    isValidWord,
    wordleGame,
  )
import Data.Char (toLower)
import System.Environment as S ( getArgs )

-- Main
main :: IO ()
main = do
  wordList <- words <$> readFile "./res/words.txt"
  args <- S.getArgs
  if "-i" `elem` args
    then interactiveGame wordList []
    else do
      putStrLn "Enter word to solve:"
      finalWord <- getAndValInput [\x -> length x == 5, (`elem` wordList), all (`elem` ['a'..'z'])]
      print $ wordleSolver wordList finalWord []

-- Make sure the user isn't fucking with me
getAndValInput :: [String -> Bool] -> IO String
getAndValInput testFuncs = do
  unSafeInput <- map toLower <$> getLine
  let test = all (\f -> f unSafeInput) testFuncs
  if test
    then return unSafeInput
    else do
      putStrLn "Not valid. Try again:"
      getAndValInput testFuncs

-- Interactive version of the game.
interactiveGame :: WordList -> [String] -> IO ()
interactiveGame wl accumGuess = do
  putStrLn $ "Guess = " ++ scoreWords wl
  putStrLn $ "Guesses so far = " ++ show accumGuess
  putStrLn "Enter your guess:"
  userGuess <- getAndValInput [\x -> length x == 5, (`elem` wl), all (`elem` ['a'..'z'])]
  putStrLn "Enter your wordle result (e, a, n):"
  userFlag <- getAndValInput [\x -> length x == 5, all (`elem` ['a', 'e', 'n'])]
  let
    guessFlag = toWordleString userGuess userFlag
    newWL = filter (`isValidWord` guessFlag) wl
  if isAllExact guessFlag
    then print $ "Final Guesses = " ++ show (accumGuess ++ [userGuess])
    else interactiveGame newWL (accumGuess ++ [userGuess])

-- Program Logic
wordleSolver :: WordList -> String -> [String] -> [String]
wordleSolver wordList finalWord accumGuess =
  if isAllExact guessFlag
    then accumGuess ++ [guess]
    else wordleSolver newWordList finalWord (accumGuess ++ [guess])
  where
    newWordList = filter (`isValidWord` guessFlag) wordList
    guessFlag = wordleGame guess finalWord
    guess = scoreWords wordList