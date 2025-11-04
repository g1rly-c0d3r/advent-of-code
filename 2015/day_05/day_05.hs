import System.Environment
import System.IO
import Data.List (isInfixOf)

-------------------------------- part 1 ------------------------------------------------------------
hasDoubleLetters::String->Bool
hasDoubleLetters [] = False
hasDoubleLetters (x:[]) = False
hasDoubleLetters (x:xs) = x==(head xs) || (hasDoubleLetters xs)

numVowels::String->Int
numVowels [] = 0
numVowels (x:xs) 
    | x `elem` "aeiouAEIOU" = 1 + (numVowels xs)
    | otherwise = 0 + numVowels xs


isNice:: String -> String
isNice xs
    -- invalid substrings
    | isInfixOf "ab" xs || isInfixOf "cd" xs || isInfixOf "pq" xs || isInfixOf "xy" xs  = "Invalid"
    -- double letters
    | (hasDoubleLetters xs) && (numVowels xs >= 3) = "nice"
    -- nothing
    | hasDoubleLetters xs = "Not enough vowels"
    | numVowels xs >= 3 = "No double letters"
    | otherwise = "Neither"
--------------------------- end part 1 -------------------------------------------------------------



---------------------------- Part 2 ----------------------------------------------------------------
---- find repeating elements of a list, and return a dictionary of the character and the number of times it repeats
--findReps::Ord a => [a] -> [(a,Int)]
--findReps xs = filter (\(_,x) -> x > 1) $ map (\l@(x:xs) -> (x,length l)) . group . sort $ xs
--
---- find repeating elements of a list and return the first element
--repIndices::Ord a => [a] -> Int -> [Int] 
--repIndices n xs = flip elemIndices xs $ fst $ (findReps xs) !! n 
--
--
--twoPair::String->Bool
--twoPar xs
--    | length xs <= 4 = False
--    | otherwise = 
--
---- find repeated elements of a list
--findReps::(Eq a)=>[a]->[(a,Int)]
--findReps xs = filter (\(_,x) -> x > 1) $ map (\l@(x:xs) -> (x,length l)) . group . sort $ xs
--
--
----------------------------- end part 2 -------------------------------------------------------------

main = do
    av <- getArgs
    rawData <- readFile $ head av

    let strings = lines rawData
    
    
    -- check output for debugging purposes
    let printAll  
            | length av > 1 = (read::String->Bool) (head $ tail av)
            | otherwise =  False


    -- part 1 
    let nice = map isNice strings
    putStrLn "~ Part 1 ~"
    if printAll then print (zip strings nice)
    else putStrLn "Pass True to see full output"

    print $ length $ filter (=="nice") nice
    putStrLn ""
    -- end part 1
    --
    -- part 2
