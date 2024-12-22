import Debug.Trace
import Data.Function
import Data.List
import Data.Maybe
import Data.Char
import Data.Ord
import Data.Bits
import Data.Tuple
import Data.String
import qualified Data.Text as T
import qualified Data.Map as M
import qualified Data.Set as S
-- moving < over ^ over v over >. 

path :: M.Map (Char, Char) String
path = M.fromList [(('A', '<'), "v<<"),
 (('A', '>'), "v"),
 (('A', 'v'), "<v"),
 (('A', '^'), "<"),
 (('<', 'A'), ">>^"),
 (('<', '^'), ">^"),
 (('<', 'v'), ">"),
 (('>', 'A'), "^"),
 (('>', '^'), "<^"),
 (('>', 'v'), "<"),
 (('^', 'A'), ">"),
 (('^', '<'), "v<"),
 (('^', '>'), "v>"),
 (('v', 'A'), "^>"),
 (('v', '>'), ">"),
 (('v', '<'), "<"),
 (('A', '0'), "<"),
 (('A', '1'), "^<<"),
 (('A', '3'), "^"),
 (('A', '4'), "^^<<"),
 (('A', '7'), "^^^<<"),
 (('A', '9'), "^^^"),
 (('0', '2'), "^"),
 (('0', 'A'), ">"),
 (('1', '7'), "^^"),
 (('1', '9'), "^^>>"),
 (('2', '9'), "^^>"),
 (('3', '1'), "<<"),
 (('3', '5'), "<^"),
 (('3', '7'), "<<^^"),
 (('4', '5'), ">"),
 (('4', '8'), "^>"),
 (('5', 'A'), "vv>"),
 (('5', '6'), ">"),
 (('6', 'A'), "vv"),
 (('7', '6'), "v>>"),
 (('7', '6'), "v>>"),
 (('7', '8'), ">"),
 (('7', '9'), ">>"),
 (('8', '0'), "vvv"),
 (('8', '9'), ">"),
 (('9', 'A'), "vvv"),
 (('9', '3'), "vv"),
 (('9', '8'), "<")]

getSequence :: Char -> Char -> String
getSequence from to
 | from == to = "A"
 | otherwise = fromJust (M.lookup (from, to) path) ++ "A"

encode :: String -> String
encode s = concat $ zipWith getSequence ('A':s) s

split :: Char -> String -> [String]
split c = map (\s -> if head s == c then tail s else s) . groupBy (\a b -> a /= c)

counter :: [String] -> M.Map String Int
counter = foldr (\s -> M.insertWith (+) s 1) M.empty

counter2 :: [(String, Int)] -> M.Map String Int
counter2 = foldr (uncurry (M.insertWith (+))) M.empty

pump :: M.Map String Int -> M.Map String Int
pump m = counter2 a
    where
        l = M.toList m
        next_gen (k, v) = map (,v) (split 'A' (encode k))
        a = concatMap next_gen l

solve :: Int -> String -> Int
solve robots s = sum' - 1
    where
        num_encoding = encode s
        parts = split 'A' num_encoding
        c = counter $ map (++"A") parts
        after = iterate pump c !! robots
        sum' = sum $ map (\(k, v) -> length k * v) $ M.toList after

main :: IO ()
main = do contents <- getContents
          let s = map (solve 2) $ lines contents
          let soln = sum $ zipWith (\ss l -> ss * read (init l)) s $ lines contents
          print soln

