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


traceLabel :: Show a => String -> a -> a
traceLabel s a = trace (s ++ " -> " ++ show a) a

readInt :: String -> Int
readInt = read

index2 :: [[a]] -> [((Int, Int), a)]
index2 a = concatMap (\(i, v) -> zipWith ((,).(i,)) [0..] v) (zip [0..] a)

possible :: [String] -> String -> Maybe [[String]]
possible towels pattern
 | null pattern = Just [[]]
 | null rtn = Nothing
 | otherwise = Just rtn
    where
        same_starts = filter (`isPrefixOf` pattern) towels
        prefixes = map (\t -> splitAt (length t) pattern) same_starts
        rtn = concat $ mapMaybe (\(prefix, remaining) ->  map (prefix:) <$> possible towels remaining) prefixes


main :: IO ()
main = do towelline <- getLine
          let towels = words $ filter (/= ',') towelline
          _ <- getLine
          patterns_ <- getContents
          let patterns = lines patterns_
          let soln = mapMaybe (possible towels) patterns
          print $ length soln

