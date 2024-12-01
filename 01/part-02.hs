-- import Data.Char
import Data.List
-- import Data.List.Utils
-- import Data.Maybe
-- import Data.Ord
-- import Data.Bits
-- import Data.String
-- import qualified Data.Text as T

readInt :: String -> Int
readInt = read

main :: IO ()
main = do contents <- getContents
          let l:r:_ =  transpose . map (map readInt . words) . lines $ contents
          let soln = sum $ zipWith (*) l $ map (\n -> length $ filter (==n) r) l
          print soln


