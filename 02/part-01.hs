-- import Data.Char
-- import Data.List
-- import Data.Maybe
-- import Data.Ord
-- import Data.Bits
-- import Data.String
-- import qualified Data.Text as T

readInt :: String -> Int
readInt = read


isSafe :: String -> Bool
isSafe s = smallChange && (up || down)
    where
        vals = map readInt $ words s
        smallChange = all (\n -> n > 0 && n < 4) $ zipWith (\a b -> abs (a - b)) vals $ tail vals
        up = and $ zipWith (<) vals $ tail vals
        down = and $ zipWith (>) vals $ tail vals


main :: IO ()
main = do contents <- getContents
          let ls = lines contents
          let safe = map isSafe ls
          print $ length $ filter id safe

