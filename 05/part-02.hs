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

sortFunc :: [(Int, Int)] -> Int -> Int -> Ordering
sortFunc rs a b | isJust $ elemIndex (a, b) rs = compare 0 1
                | isJust $ elemIndex (b, a) rs = compare 1 0
                | otherwise = error "Nope"

main :: IO ()
main = do contents <- getContents
          let ls = lines contents
          let (orders,_:lines) = splitAt (fromJust $ elemIndex "" ls) ls
          let orders' :: [(Int, Int)] = sort $ map ((\(a:b:_) -> (a, b)) . splitRead '|') orders
          let lines' :: [[Int]] = map (splitRead ',') lines
          let bad = filter (not . followsAllOrders orders') lines'
          let good' = map (sortBy (sortFunc orders')) bad
          let val = sum $ map (\l -> l!!(length l `div` 2)) good'
          print good'
          print val

