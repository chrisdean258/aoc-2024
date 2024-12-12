import Data.Function
-- import Data.Char
-- import Data.List
-- import Data.Maybe
-- import Data.Ord
-- import Data.Bits
-- import Data.String
-- import qualified Data.Text as T
import qualified Data.Map as M
import Debug.Trace

readInt :: String -> Int
readInt = read

type IMap = M.Map (Int, Int) Char


index2 :: [[a]] -> [((Int, Int), a)]
index2 a = concatMap (\(i, v) -> zipWith ((,).(i,)) [0..] v) (zip [0..] a)

insertAll :: IMap -> [((Int, Int), Char)] -> IMap
insertAll = foldl (\m (k, v) -> M.insert k v m)


findComponents :: IMap -> IMap -> ((Int, Int), Char) -> IMap
findComponents full me seed = alltogether
    where
        ((r, c), v) = seed
        ops = [ (0, 1), (0, -1), (-1, 0), (1, 0) ]
        next_seeds = [ ((r + dr, c + dc), v) | (dr, dc) <- ops, M.lookup (r + dr, c + dc) full == Just v, not $ (r + dr, c + dc) `M.member` me]
        me_with_neighbors = insertAll me next_seeds
        alltogether = foldl (findComponents full) me_with_neighbors next_seeds

calcPerim :: IMap -> Int
calcPerim m = initial - internal_perim
    where
        initial = 4 * M.size m
        ops = [ (0, 1), (0, -1), (-1, 0), (1, 0) ]
        surrounding (r, c) = sum [ 1 | (dr, dc) <- ops, M.member (r + dr, c + dc) m]
        internal_perim = sum (map (surrounding . fst) $ M.toList m)

solve m 
 | M.null m = 0
 | otherwise = area * perimeter + solve removed
    where
        item = M.findMin m
        components = findComponents m (uncurry M.singleton item) item
        area = M.size components
        perimeter = calcPerim components
        removed = M.difference m components


main :: IO ()
main = do contents <- getContents
          let indexed = M.fromList $ index2 $ lines contents
          let soln = solve indexed
          print soln

