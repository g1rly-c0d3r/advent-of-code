import Data.List

whereRepeated::String -> [[Int]]
whereRepeated xs = let abc = "abcdefghijklmnopqrstuvwxyz"
                       repeats = map (\x -> elemIndices x xs) abc
                   in filter (\xs -> (length xs)>=2) repeats 


-- Now we have the list of lists of indices of repeated numbers, we need to check to see if any are off-by-one
-- twoPair :: String -> Bool
-- twoPair xs = let repIndices = whereRepeated xs
--             in repIndices
                 

