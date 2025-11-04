import System.Environment
import System.IO
import Data.List


locs::String -> Int -> Int -> [(Int,Int)]
locs [] x y = (x,y):[]
locs "\n" x y = (x,y):[]
locs ('v':xs) x y = (x,y):(locs xs x (y-1))
locs ('>':xs) x y = (x,y):(locs xs (x+1) y)
locs ('<':xs) x y = (x,y):(locs xs (x-1) y)
locs ('^':xs) x y = (x,y):(locs xs x (y+1))

evenelem::[a]->[a]
evenelem [] = []
evenelem (o:e:xs) = e:(evenelem xs)
evenelem (o:xs) = evenelem xs

oddelem::[a]->[a]
oddelem [] = []
oddelem (o:e:xs) = o:(oddelem xs)
oddelem (o:xs) = o:(oddelem xs)

main = do 
    av <- getArgs
    rawData <- readFile $ head av

    -- part 1
    let seen = locs rawData 0 0
        numunseen = (length . nub) seen


    -- part 2
    let sSeen = locs (oddelem rawData) 0 0
        rSeen = locs (evenelem rawData) 0 0
        bothSeen = nub (sSeen ++ rSeen)

    

    -- part 1
    print numunseen
    -- part 2
    print $ length bothSeen

    
