import Data.Function
import Data.Char
import Data.List
-- import Data.Maybe
-- import Data.Ord
-- import Data.Bits
-- import Data.String
-- import qualified Data.Text as T
import qualified Data.Map as M
import qualified Data.Set as S
import Debug.Trace

type Point = (Int, Int)
type Vec = (Int, Int)

readInt :: String -> Int
readInt = read

take4 :: [a] -> [[a]]
take4 (a:b:c:_:rest) = [a,b,c]:take4 rest
take4 other = [other]

readInts :: String -> [Int]
readInts s = s & filter (\c -> isDigit c || c == ' ') & words & map readInt


solve :: [[Int]] -> (Int, Int)
solve ([x1, y1]:[x2, y2]:[tx', ty']:_)
 | common == 0 = (0, 0)
 | ra /= 0 || rb /= 0 = (0, 0)
 | otherwise = (a, b)
    where
        tx = tx' + 10000000000000
        ty = ty' + 10000000000000
        common = x1 * y2 - y1 * x2
        a' = y2 * tx - x2 * ty
        b' = x1 * ty - y1 * tx
        (a, ra) = a' `divMod` common
        (b, rb) = b' `divMod` common
solve a = error $ show a

maybeMinimum :: Ord a => [a] -> [a]
maybeMinimum a | null a = []
               | otherwise = [minimum a]


main :: IO ()
main = do contents <- getContents
          let ls = lines contents
          let ints = map readInts ls
          let probs = take4 ints
          let costs = map solve probs
          print $ sum $ map (\(a, b) -> 3 * a + b)  costs

