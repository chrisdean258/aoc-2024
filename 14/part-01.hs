import Data.Function
import Data.Char
import Data.List
import Data.Maybe
-- import Data.Ord
-- import Data.Bits
-- import Data.String
-- import qualified Data.Text as T


type Point = (Int, Int)
type Vector = (Int, Int)
type Bound = (Int, Int)
type Time = Int

predict :: Time -> Bound -> (Point, Vector) -> Point
predict t (bx, by) ((x, y),(dx, dy)) = (x', y')
    where
        x' = (x + t * dx) `mod` bx
        y' = (y + t * dy) `mod` by

parse :: String -> (Point, Vector)
parse s = read ("((" ++ l ++ "),(" ++ r ++ "))")
    where
        ((_:_:l):(_:_:r):_) = words s

findQuadrant :: Bound -> Point -> Maybe Int
findQuadrant (bx, by) (x, y)
 | x == xline || y == yline = Nothing
 | otherwise = Just (lr + 2 * ud)
    where
        xline = bx `div` 2 
        yline = by `div` 2 
        lr = if x < xline then 0 else 1
        ud = if y < yline then 0 else 1


main :: IO ()
main = do contents <- getContents
          let bounds = (101, 103)
          let probs = map parse $ lines contents
          let positions = map (predict 100 bounds) probs
          let quadrants = mapMaybe (findQuadrant bounds) positions & sort & group & map length
          print $ product quadrants

