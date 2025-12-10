import System.Environment (getArgs)

-- R is +, L is -, so make the 
-- string representation of a number 
-- either pos or neg based on that first char
dirToSign::String->String
dirToSign ('R':xs) = xs 
dirToSign ('L':xs) = '-':xs 

plusMod100::Int->Int->Int
plusMod100 x y = mod (x + y) 100 

plusDivMod100::Int->Int->(Int, Int)
plusDivMod100 x y  
        | mod (x+y) 100 == 0 = (1 + div (abs y) 100, 0)
        | x == 0  = (div (abs y) 100, mod y 100 )
        | otherwise = ( abs ( div (x+y) 100), mod (x+y) 100)

main = do
        -- get input file from the command line
        (filename:_) <- getArgs
        -- read the raw ascii stream
        raw_data <- readFile filename
        -- de-serialize
        let contents = map ((read::String->Int) . dirToSign) $ lines raw_data

        putStr "Part 1: "
        
        -- itterate through the list of rotations (starting at 50),
        -- adding the next rotation mod 100
        -- find where we land at zero
        -- and take the length of that
        let pt1 = length $ 
                  filter (==0) $
                  scanl plusMod100 50 contents
        print pt1


        putStr "Part 2: "
        -- start at 50 again
        -- the tuple is to store (#_of_full_rotations, new_position)
        -- The comment below the next line is meant to be part of this, 
        -- but for debugging reasons, is commented
        -- we will take all of the times we went past zero, 
        -- and sum them up
        let pt2 = sum $ map fst $  scanl (plusDivMod100 . snd) (0, 50)  contents
        --sum $ map fst $  
        print pt2
