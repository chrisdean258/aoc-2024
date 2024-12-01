-- import Data.Char
import Data.List
-- import Data.Maybe
-- import Data.Ord
-- import Data.Bits
-- import Data.String
-- import qualified Data.Text as T

readInt :: String -> Int
readInt = read

main :: IO ()
main = do contents <- getContents
          let ls = sum . map (\x -> abs (head x - last x)) . transpose . map sort . transpose . map (map readInt . words) . lines $ contents
          print ls


