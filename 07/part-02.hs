import Data.Function
-- import Data.Char
-- import Data.List
-- import Data.Maybe
-- import Data.Ord
-- import Data.Bits
-- import Data.String
-- import qualified Data.Text as T

readInt :: String -> Int
readInt = read

parseLine :: String -> (Int, [Int])
parseLine s = (head all, tail all)
    where all = s & map (\c -> if c == ':' then ' ' else c) & words & map readInt

concatInt :: Int -> Int -> Int
concatInt i j = show i ++ show j & read

solve :: Int -> [Int] -> Bool
solve target [val] = target == val
solve target (val:va2:rest) = solve target ((val + va2):rest) || solve target ((val * va2):rest) || solve target (concatInt val va2:rest)
solve _ _ = error "What"

main :: IO ()
main = do contents <- getContents
          let ls = lines contents
          let probs = map parseLine ls
          let solvable = map (uncurry solve) probs & zip probs & filter snd & map (fst.fst)
          print $ sum solvable

