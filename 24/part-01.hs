import Debug.Trace
import Data.Function
import Data.List
import Data.Maybe
import Data.Char
import Data.Ord
import Data.Bits
import Data.String
import qualified Data.Text as T
import Data.Map ((!))
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.IntSet as IS
import qualified Data.Vector as V

readInt :: String -> Int
readInt = read

parseInsert :: String -> M.Map String Int -> M.Map String Int
parseInsert (l1:l2:l3:':':' ':bit:_) = M.insert [l1,l2,l3] (ord bit - ord '0')

readvals :: M.Map String Int -> IO (M.Map String Int)
readvals m = do a <- getLine
                if null a then return m else readvals $ parseInsert a m

parseExecute :: M.Map String Int -> [String] -> (M.Map String Int, Bool)
parseExecute m (r1:op:r2:_:outr:_) 
 | r1 `M.notMember` m  || r2 `M.notMember` m = (m, False)
 | op == "AND" = (a, True)
 | op == "OR" = (o, True)
 | op == "XOR" = (x, True)
    where
        r1v = m ! r1
        r2v = m ! r2
        a = M.insert outr (r1v .&. r2v) m
        o = M.insert outr (r1v .|. r2v) m
        x = M.insert outr (r1v .^. r2v) m

apply :: M.Map String Int -> [[String]] -> M.Map String Int
apply m [] = m
apply m (r:rs)
 | executed = apply m' rs
 | otherwise = apply m' (rs ++ [r])
    where
        (m', executed) = parseExecute m r



main :: IO ()
main = do initial <- readvals M.empty
          contents <- getContents
          let ls = map words $ lines contents
          let m = apply initial ls
          let bits = M.toList m & filter (\(k, v) -> head k == 'z') & sort & map snd & reverse
          let val = foldl (\v b -> 2 * v + b) 0 bits
          print val

