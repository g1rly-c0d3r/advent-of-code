import System.Environment

dirToSign::String->String
dirToSign ('R':xs) = xs 
dirToSign ('L':xs) = '-':xs 

plusMod100::Int->Int->Int
plusMod100 x y = mod (x + y) 100 

plusDivMod100::Int->Int->(Int, Int)
plusDivMod100 x y = (div (x+y) 100, mod (x+y) 100)

main = do
        (filename:_) <- getArgs
        contents <- readFile filename
        putStr "Part 1: "
        let pt1 = length $ filter (==0) $ scanl (plusMod100) 50 $ map ((read::String->Int) . dirToSign) $  lines contents
        print pt1

        putStr "Part 2: "
        let pt2 = sum $ map (abs . fst) $ tail $ scanl (plusDivMod100 . snd) (0, 50) $ map ((read::String->Int) . dirToSign) $  lines contents
        
        print $ pt2
    
    
