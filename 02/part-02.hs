-- import Data.Char
-- import Data.List
-- import Data.Maybe
-- import Data.Ord
-- import Data.Bits
-- import Data.String
-- import qualified Data.Text as T

readInt :: String -> Int
readInt = read

delIndex :: Int -> [a] -> [a]
delIndex idx xs = lft ++ rgt
    where (lft, _:rgt) = splitAt idx xs
 
allPairs :: (a -> a -> Bool) -> [a] -> Bool
allPairs pred xs = and $ zipWith pred xs $ tail xs

isSafe :: [Int] -> Bool
isSafe vals = smallChange && (up || down)
    where
        smallChange = allPairs (\a b -> a /= b && abs (a - b) < 4) vals
        up = allPairs (<) vals
        down = allPairs (>) vals

isDampenedSafe :: [Int] -> Bool
isDampenedSafe vals = any (isSafe . uncurry delIndex . (,vals)) [0..(length vals - 1)]

main :: IO ()
main = do contents <- getContents
          let ls = map words $ lines contents
          let safe = map (isDampenedSafe . map readInt) ls
          print $ length $ filter id safe

