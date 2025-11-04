import System.Environment
import Data.List
import qualified Data.Set as S

data Direction = U | R | D | L deriving(Show, Read, Eq)



turn::Direction -> Direction -> Direction
turn old U = old
turn U new  
    | new == L = L
    | new == R = R
    | new == D = D
turn D new  
    | new == L = R
    | new == R = L
    | new == D = U
turn L new  
    | new == L = D
    | new == R = U
    | new == D = R
turn R new  
    | new == L = U
    | new == R = D
    | new == D = L


main = do
    (filename:_) <- getArgs
    directions <- map (filter (/=',')) . words <$> readFile filename

    putStr "Part 1: "
    print $ let (x,y) = findHQ directions U (0,0)
            in abs x + abs y

    putStr "Part 2: "
    print $ firstRepeat S.empty (cycle directions) U (0,0)



firstRepeat::S.Set (Int, Int) -> [String] -> Direction -> (Int, Int) -> S.Set (Int, Int)
firstRepeat s (str:strs) dir (x,y) 
    | S.member (x,y) s = s
    | otherwise = let newdir = turn dir (read [head str]::Direction)-- turn to new direction.
                      dist = read (tail str)::Int
                  in case newdir of 
                      U -> firstRepeat (S.union s $ S.fromList $ map (x, ) [y..y+dist]) strs newdir (x, y+dist)
                      D -> firstRepeat (S.union s $ S.fromList $ map (x, ) [y..y-dist]) strs newdir (x, y-dist)
                      R -> firstRepeat (S.union s $ S.fromList $ map ( ,y) [x..x+dist]) strs newdir (x+dist, y)
                      L -> firstRepeat (S.union s $ S.fromList $ map ( ,y) [x..x-dist]) strs newdir (x-dist, y)
firstRepeat s [] _ _ = s


findHQ::[String]->Direction->(Int,Int)->(Int,Int)
findHQ [] _ (x,y) = (x,y)
findHQ (str:strs) dir (x,y) = let newdir = turn dir (read [head str]::Direction)-- turn to new direction.
                                  dist = read (tail str)::Int
                              in case newdir of 
                                  U -> findHQ strs newdir (x, y+dist)
                                  D -> findHQ strs newdir (x, y-dist)
                                  R -> findHQ strs newdir (x+dist, y)
                                  L -> findHQ strs newdir (x-dist, y)
