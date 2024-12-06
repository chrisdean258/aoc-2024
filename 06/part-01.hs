-- import Data.Char
import Data.List
import Data.Maybe
-- import Data.Ord
-- import Data.Bits
-- import Data.String
-- import qualified Data.Text as T

readInt :: String -> Int
readInt = read

type Point = (Int, Int)
type Bound = (Int, Int)
type Direction = (Int, Int)

turnRight :: Direction -> Direction
turnRight (ud, lr) = (lr, -ud)

canMove :: Point -> Direction -> [Point] -> Bool
canMove (pr, pc) (ud, lr) obstacles = isNothing $ elemIndex (pr + ud, pc + lr) obstacles


move :: Point -> Direction -> Bound -> [Point] -> [Point]
move (pr, pc) (ud, lr) (h, w) obstacles | pr < 0 || pr >= h || pc < 0 || pc >= w = []
                                        | canMove (pr, pc) (ud, lr) obstacles = (pr, pc):move (pr + ud, pc + lr) (ud, lr) (h, w) obstacles
                                        | otherwise = (pr, pc):move (pr, pc) (turnRight (ud, lr)) (h, w) obstacles

main :: IO ()
main = do contents <- getContents
          let ls = lines contents
          let mapped = concatMap (filter (\ (_, _, a) -> a /= '.')) $ zipWith (\ r l -> zipWith (r,,) [0 .. ] l) [0 .. ] ls
          let obstacles = map (\(a, b, c) -> (a, b)) $ filter (\ (_, _, a) -> a == '#') mapped
          let (person:_) = map (\(a, b, c) -> (a, b)) $ filter (\ (_, _, a) -> a == '^') mapped
          let traveled = group $ sort $ move person (-1, 0) (length ls, length $ head ls) obstacles
          print $ length traveled

