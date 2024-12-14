import System.IO
import Data.Function
import Data.Char
import Data.List
import Data.Maybe
-- import Data.Ord
-- import Data.Bits
-- import Data.String
-- import qualified Data.Text as T
import qualified Data.Set as S


type Point = (Int, Int)
type Vector = (Int, Int)
type Bound = (Int, Int)

predict :: Bound -> (Point, Vector) -> (Point, Vector)
predict (bx, by) ((x, y),(dx, dy)) = ((x', y'),(dx, dy))
    where
        x' = (x + dx) `mod` bx
        y' = (y + dy) `mod` by

parse :: String -> (Point, Vector)
parse s = read ("((" ++ l ++ "),(" ++ r ++ "))")
    where
        ((_:_:l):(_:_:r):_) = words s

construct :: Bound -> Int -> Int -> S.Set Point -> String
construct (bx, by) idx row pts
 | row >= by = ""
 | idx >= bx = '\n':construct (bx, by) 0 (row+1) pts
 | (idx, row) `S.member` pts = 'X':continue
 | otherwise = ' ':continue
    where
        continue = construct (bx, by) (idx + 1) row pts


showIt :: Bound -> [(Int, [(Point, Vector)])] -> IO Int
showIt b ((i, pts):rest) = do let grid = construct b 0 0 $ S.fromList $ map fst pts
                              putStrLn grid
                              is_good <- getLine
                              if not $ null is_good then return i else showIt b rest

fifth :: [a] -> [a]
fifth (_:_:_:_:a:rest) = a:fifth rest



main :: IO ()
main = do handle <- openFile "input" ReadMode
          contents <- hGetContents handle
          let bounds = (101, 103)
          let probs = map parse $ lines contents
          let positions = iterate (map (predict bounds)) probs
          let good = map (\pp -> 2 > length (filter (\p -> (snd . fst) p == 0) pp)) positions
          let iterator = fifth $ map snd $ filter fst $ zip good $ zip [0..] positions
          i <- showIt bounds iterator
          print i

