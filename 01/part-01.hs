-- import Data.Char
import Data.List
-- import Data.Maybe
-- import Data.Ord
-- import Data.Bits
-- import Data.String
-- import qualified Data.Text as T

main :: IO ()
main = do contents <- getContents
          let ls :: Int = sum . map (\(a:b:_) -> abs (a - b)) . transpose . map sort . transpose . map (map read . words) . lines $ contents
          print ls


