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

expand :: Char -> String
expand '#' = "##"
expand '@' = "@."
expand '.' = ".."
expand 'O' = "[]"

process :: [String] -> M.Map Point Char
process  = M.fromList . filter (\c -> snd c /= '.') . index2 . map (concatMap expand)

move :: Map -> Point -> Vector -> (Bool, Point, Map)
move m p d
 | empty = (True, p', m)
 | wall = (False, p, m)
 | box && fst d == 0 && not move_box_lr = (False, p, m)
 | box && fst d == 0 && move_box_lr  = (True, p', M.insert p'' val lrm & M.delete p')
 | box && (not move_box_ud1 || not move_box_ud2) = (False, p, m)
 | box && move_box_ud1 && move_box_ud2 = (True, p', adjusted_post_move)
    where
        p' = p +++ d
        p'' = p' +++ d
        v = M.lookup p' m
        empty = isNothing v
        val = fromJust v
        wall = val == '#'
        box = val == ']' || val == '['
        (move_box_lr, _, lrm) = move m p' d

        (move_box_ud1, _, udm) = move m p' d
        adj_point = p +++ if val == ']' then (0, -1) else (0, 1)
        (move_box_ud2, _, udm') = move udm (adj_point +++ d) d
        partner = if val == ']' then '[' else ']'
        adjusted_post_move = udm' & M.delete p' & M.delete (adj_point +++ d) & M.insert p'' val & M.insert (adj_point +++ d +++ d) partner

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
        (br, bc) = (7, 14)
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
          let s = M.toList soln & filter (\c -> snd c == '[') & map (\((r, c), _) -> r * 100 + c) & sum
          print s

