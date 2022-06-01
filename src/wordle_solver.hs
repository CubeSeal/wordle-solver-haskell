import App.Funcs
  ( WordList,
    toWordleString,
    isAllExact,
    scoreWords,
    validWords,
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
      finalWord <- getAndValInput
        wordList [\x -> length x == 5, (`elem` wordList), all (`elem` ['a'..'z'])]
      print $ wordleSolver wordList finalWord []

-- Make sure the user isn't fucking with me
getAndValInput :: WordList -> [String -> Bool] -> IO String
getAndValInput wordList testFuncs = do
  unSafeInput <- map toLower <$> getLine
  let test = all (\f -> f unSafeInput) testFuncs
  if test
    then return unSafeInput
    else do
      putStrLn "Not a valid word. Try again:"
      getAndValInput wordList testFuncs

-- Interactive version of the game.
interactiveGame :: WordList -> [String] -> IO ()
interactiveGame wl accumGuess = do
  putStrLn $ "Guess = " ++ calcGuess
  putStrLn $ "Guesses so far = " ++ show accumGuess
  putStrLn "Enter your guess:"
  userGuess <- parseGuess <$> getAndValInput
    wl
    [\x -> length x == 5, all (`elem` ['a'..'z'])]
  putStrLn "Enter your wordle result (e, a, n):"
  userFlag <- getLine
  let
    guessFlag = toWordleString userGuess userFlag
    newWL = filter (`validWords` guessFlag) wl
  if isAllExact guessFlag
    then print $ "Final Guesses" ++ show (accumGuess ++ [userGuess])
    else interactiveGame newWL (accumGuess ++ [userGuess])
  where
    parseGuess x = case x of
      [] -> calcGuess
      y -> y
    calcGuess = scoreWords wl

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