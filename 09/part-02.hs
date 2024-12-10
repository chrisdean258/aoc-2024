import Data.Function
import Data.Char
-- import Data.List
import Data.Maybe
-- import Data.Ord
-- import Data.Bits
-- import Data.String
-- import qualified Data.Text as T
import Debug.Trace
import qualified Data.Set as S

partialSum :: Num a => [a] -> [a]
partialSum (x:xs) = x : map (+ x) (partialSum xs)
partialSum []     = []

second (x:y:xs) = x : second xs
second (x:_) = [x]
second _ = []

solve :: [(Int, Int)] -> S.Set (Int, Int) -> Int -> [(Int, Int, Int)]
solve ((idx, size):blocks_rev) free block_idx
    | movable && free_size == size = (free_idx, free_size, block_idx):solve blocks_rev removed (block_idx - 1)
    | movable = (free_idx, size, block_idx):solve blocks_rev added (block_idx - 1)
    | otherwise = (idx, size, block_idx):solve blocks_rev free (block_idx - 1)
        where
            first = take 1 $ filter (\(i, s) -> s >= size) $ S.toList free
            movable = length first == 1 && free_idx < idx
            (free_idx, free_size) = head first
            removed = S.delete (free_idx, free_size) free
            added = S.insert (free_idx + size, free_size - size) removed
solve _ _ _ = []

main :: IO ()
main = do line <- getLine
          let blocks = map (\c -> ord c - ord '0') line
          let partials = partialSum blocks
          let blocks' = zip (0:partials) blocks
          let free = S.fromList $ second $ tail blocks'
          let occupied = second blocks'
          let rev = reverse occupied
          let block_mappings = solve rev free (length rev -1)
          let soln =sum $ map (\(idx, run, val) -> val * sum [idx..idx + run - 1]) block_mappings
          print soln

