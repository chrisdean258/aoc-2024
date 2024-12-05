-- import Data.Char
import Data.List
import Data.Maybe
-- import Data.Ord
-- import Data.Bits
-- import Data.String
-- import qualified Data.Text as T

readInt :: String -> Int
readInt = read

splitRead :: Read a => Char -> String -> [a]
splitRead sep = map read . words . map (\c -> if c == sep then ' ' else c)

followsOrder :: (Int, Int) -> [Int] -> Bool
followsOrder (f, l) (x:xs) | x == f = True
                           | x == l = isNothing $ elemIndex f xs
                           | otherwise = followsOrder (f, l) xs
followsOrder _ _ = True

followsAllOrders :: [(Int, Int)] -> [Int] -> Bool
followsAllOrders orders line = all (`followsOrder` line) orders


main :: IO ()
main = do contents <- getContents
          let ls = lines contents
          let (orders,_:lines) = splitAt (fromJust $ elemIndex "" ls) ls
          let orders' :: [(Int, Int)] = sort $ map ((\(a:b:_) -> (a, b)) . splitRead '|') orders
          let lines' :: [[Int]] = map (splitRead ',') lines
          let good = filter (followsAllOrders orders') lines'
          let val = sum $ map (\l -> l!!(length l `div` 2)) good
          print val

