import Data.Function
-- import Data.Char
import Data.List
-- import Data.Maybe
-- import Data.Ord
-- import Data.Bits
-- import Data.String
-- import qualified Data.Text as T

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




main :: IO ()
main = do line <- getLine
          let stones = map readInt $ words line
          let stones' = iterate (concatMap blinkOne) stones !! 25

          print $ length stones'

