-- import Data.Char
import Data.List
-- import Data.Maybe
-- import Data.Ord
-- import Data.Bits
-- import Data.String
-- import qualified Data.Text as T

readInt :: String -> Int
readInt = read

diagsHelp :: [String] -> Int -> String
diagsHelp s i = map (\n -> s!!n!!(i-n)) [0..i]

diags :: [String] -> [String]
diags s = map (diagsHelp s) [0..length s - 1] ++ map (diagsHelp $ reverse $ map reverse s) [0..length s - 2]


countXMAS :: String -> Int
countXMAS [] = 0
countXMAS ('X':'M':'A':'S':rest) = 1 + countXMAS ('S':rest)
countXMAS ('S':'A':'M':'X':rest) = 1 + countXMAS ('X':rest)
countXMAS (s:ss) = countXMAS ss

main :: IO ()
main = do contents <- getContents
          let ls = lines contents
          let c1 = sum $ map countXMAS ls
          let c2 = sum $ map countXMAS $ transpose ls
          let c3 = sum $ map countXMAS $ diags ls
          let c4 = sum $ map countXMAS $ diags $ reverse ls
          print (c1 + c2 + c3 + c4)

