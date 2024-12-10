import Data.Function
import Data.Char
-- import Data.List
-- import Data.Maybe
-- import Data.Ord
-- import Data.Bits
-- import Data.String
-- import qualified Data.Text as T
import qualified Data.Map as M

readInt :: String -> Int
readInt = read

index2 :: [[Char]] -> [((Int, Int), Int)]
index2 a = concatMap (\(i, v) -> zipWith ((,).(i,)) [0..] (map (\c -> ord c - ord '0') v)) (zip [0..] a)

countPaths :: M.Map (Int, Int) Int -> Int -> (Int, Int) -> Int
countPaths height_map cur_height (r, c) 
 | cur_height == 9 = 1
 | otherwise = sum $ map (countPaths height_map (cur_height + 1)) higher
    where
        surrounding = [(r + 1, c), (r, c + 1), (r - 1, c), (r, c - 1) ]
        higher = [a | a <- surrounding, a `M.lookup` height_map == Just (cur_height + 1)]

main :: IO ()
main = do contents <- getContents
          let hl = index2 $ lines contents
          let heights = M.fromList $ index2 $ lines contents
          let starts = map fst $ filter (\(_, v) -> v == 0) hl
          let soln = sum $ map (countPaths heights 0) starts
          print soln

