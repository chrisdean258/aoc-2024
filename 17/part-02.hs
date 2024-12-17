import Debug.Trace
import Data.Function
import Data.List
import Data.Maybe
import Data.Char
import Data.Ord
import Data.Bits
import Data.String
import qualified Data.Text as T
import qualified Data.Map as M
import qualified Data.Set as S

-- Turns out this is not a problem about termination, just extracing 3 bits at a time

readInt :: String -> Int
readInt = read

decode :: Int -> Int -> Int -> Int -> Int 
decode _ _ _ 0 = 0
decode _ _ _ 1 = 1
decode _ _ _ 2 = 2
decode _ _ _ 3 = 3
decode a _ _ 4 = a
decode _ b _ 5 = b
decode _ _ c 6 = c

run :: Int -> Int -> Int -> Int -> Int -> Int -> [Int] -> (Int, Int, Int, Int, [Int])
run a b c operand instr pc output
 | instr == 0 = (a `shiftR` combo, b, c, pc+2, output)
 | instr == 1 = (a, b `xor` operand, c, pc+2, output)
 | instr == 2 = (a, combo `mod` 8, c, pc+2, output)
 | instr == 3 = (a, b, c, if a /= 0 then operand else pc + 2, output)
 | instr == 4 = (a, b `xor` c, c, pc + 2, output)
 | instr == 5 = (a, b, c, pc + 2, output ++ [combo `mod` 8])
 | instr == 6 = (a, a `shiftR` combo, c, pc+2, output)
 | instr == 7 = (a, b, a `shiftR` combo,  pc+2, output)
    where
        combo = decode a b c operand

interpret :: Int -> Int -> Int -> Int -> [Int] -> [Int]-> (Int, Int, Int, Int, [Int])
interpret a b c pc instr output
 | pc >= length instr = (a, b, c, pc, output)
 | otherwise = interpret a' b' c' pc' instr output'
    where 
        (ist:operand:_) = drop pc instr
        (a', b', c', pc', output') = run a b c operand ist pc output


solve :: Int -> Int -> Int -> [Int] -> Int -> Int
solve a b c instr back_match
 | back_match == length instr = soln
 | otherwise = solve (8*soln) b c instr (back_match + 1)
    where 
        desired = reverse $ take back_match $ reverse instr
        soln = fst $ head $ filter (\(i, (_,_,_,_,v)) -> v == desired) $ zip [a..] $ map (\i -> interpret i b c 0 instr []) [a..a+8]

main :: IO ()
main = do contents <- getContents
          let (a:b:c:instr) = map readInt $ words $ map (\c -> if isDigit c then c else ' ') contents
          let i = solve 0 b c instr 1
          print i

