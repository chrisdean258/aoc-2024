import Debug.Trace
import Data.Function
import Data.List
import Data.Maybe
import Data.Char
import Data.Ord
import Data.Bits
import Data.String
import Text.Printf
import qualified Data.Text as T
import Data.Map ((!))
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.IntSet as IS
import qualified Data.Vector as V

readInt :: String -> Int
readInt = read

type Map = M.Map (String, String, String) String
type BackMap = M.Map String (String, String, String)

parseInsert :: String -> M.Map String Int -> M.Map String Int
parseInsert (l1:l2:l3:':':' ':bit:_) = M.insert [l1,l2,l3] (ord bit - ord '0')

readvals :: M.Map String Int -> IO (M.Map String Int)
readvals m = do a <- getLine
                if null a then return m else readvals $ parseInsert a m

buildMap :: (Map, BackMap) -> [String] -> (Map, BackMap)
buildMap (m, bm) (r1:op:r2:_:out:_) = (m', bm')
    where
        m' = M.insert (r1, op, r2) out $ M.insert (r2, op, r1) out m
        bm' = M.insert out (r1, op, r2) $ M.insert out (r2, op, r1) bm

-- x00 AND y00 -> prev_carry

-- x01 XOR y01 -> ib1
-- prev_carry XOR ib1 -> z01
-- y01 AND x01 -> ic1
-- prev_carry AND ib1 -> ic2
-- ic1 OR ic2 -> fc


symDiff :: Eq a => [a] -> [a] -> [a]
symDiff setA setB = [c | c <- setA, c `notElem` setB] ++ [c | c <- setB, c `notElem` setA]


deviant :: BackMap -> Map -> Int -> String -> (String, [String])
deviant bm m i prev_carry = undefined
    where
        xnode :: String = printf "x%02d" i
        ynode :: String = printf "y%02d" i
        znode :: String = printf "z%02d" i
        ib1 = m ! (xnode, "XOR", ynode)
        (expt_bitnode, expt_op, expt_carry_node) = bm ! znode
        bad = symDiff [expt_carry_node, expt_bitnode] [ib1, prev_carry]

        initial_carry_1 = m ! (xnode, "AND", ynode)
        ic2 = M.lookup (prev_carry, "AND", ib1) m
        bad' = case ic2 of
                Nothing -> error "ahh"
                Just _ -> bad




main :: IO ()
main = do initial <- readvals M.empty
          contents <- getContents
          let ls = map words $ lines contents
          let (m, bm) = foldl buildMap (M.empty, M.empty) ls
          print m

