import Data.Function
-- import Data.Char
import Data.List
-- import Data.Maybe
-- import Data.Ord
-- import Data.Bits
-- import Data.String
-- import qualified Data.Text as T

type Point = (Int, Int)
type Vec = (Int, Int)
type Bound = (Int, Int)


getHalfAntinodes :: Point -> Vec -> Bound -> [Point]
getHalfAntinodes (r, c) (dr, dc) (rows, cols) | r < 0 || r >= rows || c < 0 || c >= cols = []
                                              | otherwise = (r, c):getHalfAntinodes (newr, newc) (dr, dc) (rows, cols)
                                              where
                                                newr = r + dr
                                                newc = c + dc


getAntinodes :: Bound -> (Point, Point) -> [Point]
getAntinodes bound ((r1, c1), (r2, c2)) = getHalfAntinodes (r1, c1) (dr, dc) bound ++ getHalfAntinodes (r2, c2) (-dr, -dc) bound
    where
        dr = r1 - r2
        dc = c1 - c2


main :: IO ()
main = do contents <- getContents
          let ls = lines contents
          let indexed = concatMap (filter (\v -> snd v /= '.') . (\(i, v) -> zipWith (\j c -> ((i, j), c)) [0 .. ] v)) (zip [0 .. ] ls)
          let allPairs = [(fst a, fst b) | a <- indexed, b <- indexed, snd a == snd b && a /= b]
          let bound = (length ls, length $ head ls)
          let antinodes = concatMap (getAntinodes bound) allPairs
          print $ length $ group $ sort antinodes

