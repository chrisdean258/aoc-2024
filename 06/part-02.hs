-- import Data.Char
import Data.List
import Data.Maybe
-- import Data.Ord
import qualified Data.Set as S
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

canMove :: Point -> Direction -> S.Set Point -> Bool
canMove (pr, pc) (ud, lr) = not . S.member (pr + ud, pc + lr)


move :: Point -> Direction -> Bound -> S.Set Point -> [Point]
move (pr, pc) (ud, lr) (h, w) obstacles | pr < 0 || pr >= h || pc < 0 || pc >= w = []
                                        | canMove (pr, pc) (ud, lr) obstacles = (pr, pc):move (pr + ud, pc + lr) (ud, lr) (h, w) obstacles
                                        | otherwise = (pr, pc):move (pr, pc) (turnRight (ud, lr)) (h, w) obstacles

testCycle :: Point -> Direction -> Bound -> S.Set Point -> S.Set (Point, Direction) -> Bool
testCycle (pr, pc) (ud, lr) (h, w) obstacles path | pr < 0 || pr >= h || pc < 0 || pc >= w = False
                                                  | S.member ((pr, pc), (ud, lr)) path = True
                                                  | canMove (pr, pc) (ud, lr) obstacles = testCycle (pr + ud, pc + lr) (ud, lr) (h, w) obstacles extpath
                                                  | otherwise = testCycle (pr, pc) (turnRight (ud, lr)) (h, w) obstacles extpath
                                                  where
                                                    extpath = S.insert ((pr, pc), (ud, lr)) path

main :: IO ()
main = do contents <- getContents
          let ls = lines contents
          let mapped = concatMap (filter (\ (_, _, a) -> a /= '.')) $ zipWith (\ r l -> zipWith (r,,) [0 .. ] l) [0 .. ] ls
          let obstacles = S.fromList $ map (\(a, b, c) -> (a, b)) $ filter (\ (_, _, a) -> a == '#') mapped
          let (person:_) = map (\(a, b, c) -> (a, b)) $ filter (\ (_, _, a) -> a == '^') mapped
          let _:traveled = map head $ group $ sort $ move person (-1, 0) (length ls, length $ head ls) obstacles -- pop off the first item cause we cant put something where the gaurd alread is
          let cycles = filter id $ map (\o -> testCycle person (-1, 0) (length ls, length$head ls) (S.insert o obstacles) S.empty) traveled
          print $ length cycles

