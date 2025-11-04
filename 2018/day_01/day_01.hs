import System.Environment
import System.IO
import Data.List
import Data.Maybe
import qualified Data.Set as S

firstRepeat::(Ord a)=>[a]-> S.Set a -> Maybe a
firstRepeat [] _ = Nothing
firstRepeat (x:xs) s
    | S.member x s = Just x
    | otherwise = firstRepeat xs (S.insert x s)


main = do
    -- input processing
    (filename:_) <- getArgs
    contents <- readFile filename
    let deltas = map (\x ->  read (delete '+' x)::Int ) $ lines contents

    -- part 1
    putStr "Part 1: "
    print $ sum deltas

    -- part 2
    putStr "Part 2: "
    let infdeltas = cycle deltas
        infFreqs = scanl1 (+) infdeltas

    print $ firstRepeat infFreqs S.empty




    

