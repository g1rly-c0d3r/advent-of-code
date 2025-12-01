import System.Environment
import Data.Char (digitToInt)

main = do 
    (filename:_) <- getArgs
    contents <- fmap digitToInt . init <$> readFile filename
    let circ_contents = contents ++ [head contents]


    putStr "Part 1: "
    print $ equals circ_contents 0

    putStr "Part 2: "
    print $ equals' contents


equals::(Eq a, Num a)=>[a]->a->a
equals [] n = n
equals [x] n = n
equals (x:y:s) n
    | x == y = equals (y:s) n+x
    | otherwise = equals (y:s) n


equals'::[Int]->Int
equals' xs = let middle = length xs `div` 2 
                 (first', second') = splitAt middle xs
                 in sum $ map ((2*) . fst) $ filter (uncurry (==)) $ zip first' second'
            

