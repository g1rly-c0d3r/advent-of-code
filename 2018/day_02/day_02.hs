import System.Environment
import System.IO
import Data.List

twopeats::(Ord a)=>[a]->Bool
twopeats xs = any (\ys -> (length ys) == 2) $ (group . sort) xs

threepeats::(Ord a)=>[a]->Bool
threepeats xs = any (\ys -> (length ys) == 3) $ (group . sort) xs

main = do 
    (filename:_) <- getArgs
    keys <- fmap lines $ readFile filename 

    -- part 1
    putStr "Part 1: "
    let numdoubles = length $ takeWhile (==True) $ reverse .  sort $ map (twopeats) keys
        numtriples = length $ takeWhile (==True) $ reverse .  sort $ map (threepeats) keys

    putStrLn $ (show numdoubles) ++ " * " ++ (show numtriples) ++ " = " ++ (show $ numdoubles*numtriples)
