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

possible :: [String] -> String -> [Int]
possible towels [] = [1]
possible towels (p:pattern) = memoized ++ [val]
    where
        memoized = possible towels pattern
        same_starts = filter (`isPrefixOf` (p:pattern)) towels
        lens = map (\t -> length (p:pattern) - length t) same_starts
        val = sum $ map (memoized !!) lens


main :: IO ()
main = do towelline <- getLine
          let towels = words $ filter (/= ',') towelline
          _ <- getLine
          patterns_ <- getContents
          let patterns = lines patterns_
          let soln = map (last . possible towels) patterns
          print $ sum soln

