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

surrounding :: Point -> [Point]
surrounding (x, y) = [(x + dx, y + dy) | (dx, dy) <- [(-1, 0), (1, 0), (0, 1), (0, -1)]]

insertAll :: Ord a => M.Map a b -> [(a, b)] -> M.Map a b
insertAll m [] = m
insertAll m ((a,b):rest) = insertAll (M.insert a b m) rest


solve :: (Int, Int) -> S.Set Point -> M.Map Point Int -> M.Map (Int, Int) Point -> Int -> (M.Map Point Int, M.Map (Int, Int) Point)
solve (bx, by) illegal processed q unique
 | M.null q = (processed, q)
 | otherwise = solve (bx, by) illegal processed' q'' (unique + 4)
    where
        (((c, _), (px, py)), q') = M.deleteFindMin q
        isValid (x, y) = (x, y) `M.notMember` processed &&  (x, y) `S.notMember` illegal &&  x >= 0 &&  y >= 0 &&  x <= bx &&  y <= by
        newpts = filter isValid $ surrounding (px, py)
        q'' = insertAll q' $ zip (map (c+1,) [unique..])  newpts
        processed' = insertAll processed $ map (,c+1) newpts 

search :: Int -> Int -> [Point] -> (Int, Int) -> Int
search low high pts bound 
 | isNothing v && mid == high = mid 
 | isJust v && mid == low = mid + 1
 | isJust v = search mid high pts bound
 | otherwise = search low mid pts bound
    where
        mid = (low + high) `div` 2
        v = M.lookup bound $ fst $ solve bound (S.fromList $ take mid pts) M.empty (M.singleton (0,0) (0,0)) 1


main :: IO ()
main = do contents <- getContents
          let pts :: [Point] = lines contents & map (\l -> '(':l ++ ")" & read) 
          let target = (70, 70)
          -- The solution is the minimum number it took from the list to break
          -- the searchability which is one more than the index of the element
          -- it took to break the searchability
          let soln = search 1024 (length pts) pts target
          let soln_idx = soln - 1
          print (pts!!soln_idx)


