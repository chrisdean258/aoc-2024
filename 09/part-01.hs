import Data.Function
import Data.Char
-- import Data.List
import Data.Maybe
-- import Data.Ord
-- import Data.Bits
-- import Data.String
-- import qualified Data.Text as T
import Debug.Trace

type FrIt = ([Int], [Int], [Int], Int, Int, Int)

readInt :: String -> Int
readInt = read

second (x:y:xs) = x : second xs
second (x:_) = [x]
second _ = []

nextIt :: FrIt -> Maybe (FrIt, Int, Int)
nextIt (b:blocks, r:rev, f:free, index, rindex, length)
    | index > rindex = Nothing
    | index == rindex = Just ((blocks, r:rev, free, index+1, rindex, length), rindex, r)
    | f == 0 = Just ((blocks, r:rev, free, index+1, rindex, length), index, b)
    | r > f = Just ((b:blocks, (r-f):rev, 0:free, index, rindex, length), rindex, f)
    | r == f = Just ((b:blocks, rev, 0:free, index, rindex - 1, length), rindex, f)
    | r < f = Just((b:blocks, rev, (f - r):free, index, rindex - 1, length), rindex, r)

nextIt (blocks, rev, free, index, rindex, length) = Nothing

pump :: FrIt -> [(Int, Int)]
pump frit | isJust val = (idx, count):pump frit'
          | otherwise = []
    where
        val = nextIt frit
        (frit', idx, count) = fromJust val

solve :: [Int] -> [(Int, Int)]
solve i = pump frit
    where
        filled = second i
        rev = reverse filled
        free = second $ tail i
        l = length filled
        frit = (filled, rev, 0:free, 0, l - 1, l)

resolve :: [(Int, Int)] -> Int-> Int
resolve ((v, c):xs) i = sum (map (v*) [i..i+c-1]) + resolve xs i'
    where
        i' = i + c
resolve _ i = 0


main :: IO ()
main = do line <- getLine
          let blocks = map (\c -> ord c - ord '0') line
          let inOrder = solve blocks
          print $ resolve inOrder 0

