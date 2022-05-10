import App.Funcs

main :: IO ()
main = do
    wordsRaw <- readFile "./res/words.txt"
    let wordList = words wordsRaw
    print wordList