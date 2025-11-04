import System.Environment
import System.IO


-- part 1
whatFloor:: String -> Integer
whatFloor "\n" = 0
whatFloor ('(':xs) = whatFloor xs + 1
whatFloor (')':xs) = whatFloor xs - 1

--part 2
-- Takes a string of parens and a starting floor
-- returns when the first occurence of being in the basement
whenBasement:: String -> Integer -> Integer -> Maybe Integer
whenBasement "\n" _ _ = Nothing
whenBasement ('(':xs) floor n = whenBasement xs (floor+1) (n+1)
whenBasement (')':xs) floor n
    | floor > 0 = whenBasement xs (floor-1) (n+1)  
    | floor <= 0 = Just (n+1)




main = do
    filename <- fmap head getArgs
    -- Read input from 1st cl arg
    handle <- openFile filename ReadMode
    parens <- hGetContents handle

    -- part 1
    print $ whatFloor parens

    print $ whenBasement parens 0 0

    hClose handle
