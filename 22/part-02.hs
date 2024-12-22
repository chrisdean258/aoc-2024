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

readInt :: String -> Int
readInt = read

index2 :: [[a]] -> [((Int, Int), a)]
index2 a = concatMap (\(i, v) -> zipWith ((,).(i,)) [0..] v) (zip [0..] a)

mix :: Int -> Int -> Int
mix = xor

prune :: Int -> Int
prune = (`mod` 16777216)

next :: Int -> Int
next s = s3
    where
       s1 = prune $ mix s (64 * s)
       s2 = prune $ mix s1 (s1 `div` 32)
       s3 = prune $ mix s2 (2048 * s2)

addSums :: M.Map (Int, Int, Int, Int) Int -> [Int] -> [Int] -> M.Map (Int, Int, Int, Int) Int
addSums m (p1:p2:p3:p4:p5:ps) (n1:n2:n3:n4:rest) = addSums m' (p2:p3:p4:p5:ps) (n2:n3:n4:rest)
    where 
        m' = if (n1, n2, n3, n4) `M.member` m then m else M.insert (n1, n2, n3, n4) p5 m
addSums m _ _ = m

main :: IO ()
main = do contents <- getContents
          let nums = map readInt $ lines contents
          let nums' :: [[Int]] = map (map (`mod` 10) . take 2000 . iterate next) nums
          let diffs = map (\ns -> zipWith (-) (tail ns) ns) nums'
          let ms = zipWith (addSums M.empty) nums'  diffs
          let all = groupBy ((==) `on` fst) $ sortBy (compare `on` fst) $ concatMap M.toList ms
          -- print $ maximumBy (compare `on` sum . map snd) all
          print $ sum . map snd $ maximumBy (compare `on` sum . map snd) all

