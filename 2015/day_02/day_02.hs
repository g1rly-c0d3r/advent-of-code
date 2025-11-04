import System.Environment
import System.IO
import Data.List.Split
import Data.List

strToInt:: [[String]] -> [[Int]]
strToInt [] = []
strToInt (x:xs) = (map (read::String->Int) x):(strToInt xs)


smallestSideArea::[Int] -> Int
smallestSideArea xs = let (smest:smer:sm) = sort xs
                  in  smest * smer

smallestSidePerim::[Int] -> Int
smallestSidePerim xs = let (smest:smer:sm) = sort xs
                  in  2*smest + 2*smer

main = do
    av <- getArgs
    handle <- openFile (head av) ReadMode
    rawDims <- hGetContents handle

    -- get each present
    let dims = (strToInt . map (wordsBy (=='x')) . lines) rawDims
        -- part 1
        area = map (\[l,w,h] -> 2*l*w + 2*w*h + 2*h*l + (smallestSideArea [l,w,h])) dims
        -- part 2
        ribs = map (\[l,w,h] -> l*w*h + (smallestSidePerim [l,w,h])) dims

    -- part 1
    print $ sum area
    --part 2
    print $ sum ribs

    hClose handle
