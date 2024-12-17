import Debug.Trace
import Data.Function
import Data.List
import Data.Maybe
import Data.Char
import Data.Ord
import Data.Bits
import Data.String
import qualified Data.Text as T
import qualified Data.Map as M
import qualified Data.Set as S

type Point = (Int, Int)
type Direction = (Int, Int)

directions :: [Direction]
directions = [(0,1), (1, 0), (0,-1), (-1,0)]

rotateDir :: Direction -> Direction
rotateDir (ud, lr) = (-lr, ud)

rotateDir' :: Direction -> Direction
rotateDir' (ud, lr) = (lr, -ud)


type Map = M.Map (Int, Int) (Point, Direction)
type Set = S.Set (Point, Direction)

(+++) :: Point -> Direction -> Point
(+++) (px, py) (dx, dy) = (px + dx, py + dy)


readInt :: String -> Int
readInt = read

index2 :: [[a]] -> [((Int, Int), a)]
index2 a = concatMap (\(i, v) -> zipWith ((,).(i,)) [0..] v) (zip [0..] a)

update :: Point -> Direction -> Set -> Map -> (Int, Int) -> Map
update p d s m v
 | seen = m
 | otherwise = m'
    where
        seen = S.notMember (p, d) s
        m' = M.insert v (p, d) m

solve :: Set -> Point -> Map -> Int -> Int
solve points target m c
 | p == target = v
 | (p, d) `S.notMember` points = solve points target m' (c + 3)
 | otherwise = solve points' target m'' (c + 3)
    where
        (((v, _), (p, d)), m') = M.deleteFindMin m
        points' = S.delete (p, d) points
        s_m = update (p +++ d) d points' m' (v + 1, c + 1)
        r_m = update p (rotateDir d) points' s_m (v + 1000, c + 2)
        m'' = update p (rotateDir' d) points' r_m (v + 1000, c + 3)
        

main :: IO ()
main = do contents <- getContents
          let map_ = index2 $ lines contents
          let map_points :: Set = map_ & filter (\c -> snd c == '.' || snd c == 'S' || snd c == 'E') & concatMap (\v -> map (fst v,) directions) & S.fromList
          let (start, _) = map_ & filter (\c -> snd c == 'S') & head
          let (target, _) = map_ & filter (\c -> snd c == 'E') & head
          let soln = solve map_points target (M.singleton (0,0) (start, (0, 1))) 0
          print soln

