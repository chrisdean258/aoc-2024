import Data.Function
-- import Data.Char
import Data.List
-- import Data.Maybe
-- import Data.Ord
-- import Data.Bits
-- import Data.String
-- import qualified Data.Text as T
import qualified Data.Map as M

readInt :: String -> Int
readInt = read

blinkOne :: Int -> [Int]
blinkOne s
 | s == 0 = [1]
 | even l = [readInt left, readInt right]
 | otherwise = [s * 2024]
    where
        ss = show s
        l = length ss
        (left, right) = splitAt (l `div` 2) ss

counter :: Ord a => [a] -> M.Map a Int
counter = M.fromList . map (\v -> (head v, length v)) . group . sort

mergeReduce :: [(Int, Int)] -> [(Int, Int)]
mergeReduce ((v1, c1):(v2, c2):rest)
 | v1 == v2 = mergeReduce ((v1, c1 + c2):rest)
 | otherwise = (v1, c1):mergeReduce ((v2, c2):rest)
mergeReduce l = l

blink :: M.Map Int Int -> M.Map Int Int
blink m = M.fromList $ mergeReduce $ sort $ concatMap (\(v, c) -> map (,c) (blinkOne v)) $ M.toList m


main :: IO ()
main = do line <- getLine
          let stones = map readInt $ words line
          let stones' = counter stones
          let mapping = iterate blink stones' !! 75
          let s = mapping & M.toList & map snd & sum

          print s

