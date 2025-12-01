import System.Environment (getArgs)
import Data.List.Extra (linesBy, wordsBy)

main = do
    (filename:_) <- getArgs
    contents <- readFile filename
    let nums = map (map (read::String->Int) . wordsBy (=='\t')) (lines contents)

    putStr "Part 1: "
    print $ sum $ map (\xs -> maximum xs - minimum xs) nums

    putStr "Part 2: \n"


