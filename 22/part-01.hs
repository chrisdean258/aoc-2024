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

main :: IO ()
main = do contents <- getContents
          let nums = map readInt $ lines contents
          let nums' = map (\n -> iterate next n !! 2000) nums
          print $ sum nums'

