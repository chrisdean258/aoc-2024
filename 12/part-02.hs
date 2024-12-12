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

-- Calculate the perimeter by considering the delta in perimeter from the 3
-- that are above and one directly to the left of the added square
-- This simualtes building it up from top left across the row then down
-- The constants were determined by hand, and the mapping is the left block at bit 0
-- and the top right as bit 1, above as bit 2 and above left as bit 3
calcPerim :: IMap -> Int
calcPerim m = perim
    where
        ops = [ (-1, -1), (-1, 0), (-1, 1), (0, -1) ] 
        code (r, c) = sum $ map snd $ filter fst $ zip [ M.member (r + dr, c + dc) m | (dr, dc) <- ops] [8, 4, 2, 1]
        decode n = [4,0,4,0,0,-2,2,0,4,2,4,2,2,-2,4,0]!!n
        perim = sum $ map (decode . code . fst) $ M.toList m

solve m 
 | M.null m = 0
 | otherwise = perimeter*area + solve removed
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

