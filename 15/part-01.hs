import Data.Function
-- import Data.Char
-- import Data.List
import Data.Maybe
-- import Data.Ord
-- import Data.Bits
-- import Data.String
-- import qualified Data.Text as T
import qualified Data.Map as M
import Debug.Trace



type Vector = (Int, Int)
type Point = (Int, Int)
type Map = M.Map Point Char

index2 :: [[a]] -> [((Int, Int), a)]
index2 a = concatMap (\(i, v) -> zipWith ((,).(i,)) [0..] v) (zip [0..] a)

readMap :: IO [String]
readMap = do line <- getLine
             if null line then return [line] else do { map <- readMap;  return (line:map) }

decodeDir :: Char -> Vector
decodeDir '<' = (0, -1)
decodeDir '>' = (0, 1)
decodeDir '^' = (-1, 0)
decodeDir 'v' = (1, 0)

(+++) :: Point -> Vector -> Point
(+++) (r, c) (dr, dc) = (r + dr, c + dc)

process :: [String] -> M.Map Point Char
process  = M.fromList . filter (\c -> snd c /= '.') . index2

move :: Map -> Point -> Vector -> (Bool, Point, Map)
move m p d
 | empty = (True, p', m)
 | wall = (False, p, m)
 | box && not move_box = (False, p, m)
 | box && move_box = (True, p', M.insert p'' 'O' m' & M.delete p')
    where
        p' = p +++ d
        p'' = p' +++ d
        empty = not $ p' `M.member` m
        wall =  M.lookup p' m == Just '#'
        box =  M.lookup p' m == Just 'O'
        (move_box, _, m') = move m p' d

solve :: Point -> [Vector] -> Map -> Map
solve p (d:ds) m = solve p' ds m'
    where
        (_, p', m') = move m p d
solve _ _ m = m


construct :: Point -> Int -> Int -> Map -> String
construct p idx row m
 | row >= br = ""
 | idx >= bc = '\n':construct p 0 (row+1) m
 | (row, idx) == p = '@':continue
 | isNothing val = ' ':continue
 | otherwise = fromJust val:continue
    where
        (br, bc) = (9, 9)
        val = M.lookup (row, idx) m
        continue = construct p (idx + 1) row m


main :: IO ()
main = do map_ <- readMap
          contents <-  getContents
          let directions = concat $ lines contents
          let directions' = map decodeDir directions
          let map' = process map_
          let (robotpos, _) = head $ filter (\c -> snd c == '@') $ M.toList map'
          let map'' = M.delete robotpos map'
          let soln = solve robotpos directions' map''
          let s = M.toList soln & filter (\c -> snd c == 'O') & map (\((r, c), _) -> r * 100 + c) & sum
          print s

