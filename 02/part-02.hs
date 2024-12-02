-- import Data.Char
-- import Data.List
-- import Data.Maybe
-- import Data.Ord
-- import Data.Bits
-- import Data.String
-- import qualified Data.Text as T

readInt :: String -> Int
readInt = read

doDrops1 :: Int -> [a] -> [a]
doDrops1 i [] = []
doDrops1 i (v:vs) | i == 0 = vs
                  | otherwise = v:doDrops1 (i-1) vs


doDropsInt :: Int -> [a] -> [[a]]
doDropsInt i vs | i == length vs = []
                | otherwise = doDrops1 i vs:doDropsInt (i+1) vs

doDrops :: [a] -> [[a]]
doDrops = doDropsInt 0


isSafe :: [Int] -> Bool
isSafe vals = smallChange && (up || down)
    where
        smallChange = all (\n -> n > 0 && n < 4) $ zipWith (\a b -> abs (a - b)) vals $ tail vals
        up = and $ zipWith (<) vals $ tail vals
        down = and $ zipWith (>) vals $ tail vals

isSafeString :: String -> Bool
isSafeString s = safety
    where
        vals = map readInt $ words s
        dropouts = doDrops vals
        safety = any isSafe dropouts


main :: IO ()
main = do contents <- getContents
          let ls = lines contents
          let safe = map isSafeString ls
          print $ length $ filter id safe

