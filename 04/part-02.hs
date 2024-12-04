-- import Data.Char
-- import Data.List
-- import Data.Maybe
-- import Data.Ord
-- import Data.Bits
-- import Data.String
-- import qualified Data.Text as T

triples :: [a] -> [(a, a, a)]
triples ls = zip3 ls (tail ls) (tail $ tail ls)

isMAS :: (Char, Char, Char) -> (Char, Char, Char) -> (Char, Char, Char) -> Int
isMAS ('M', _, 'M') (_, 'A', _) ('S', _, 'S') = 1
isMAS ('M', _, 'S') (_, 'A', _) ('M', _, 'S') = 1
isMAS ('S', _, 'M') (_, 'A', _) ('S', _, 'M') = 1
isMAS ('S', _, 'S') (_, 'A', _) ('M', _, 'M') = 1
isMAS _ _ _ = 0

isMASl :: [([(Char, Char, Char)], [(Char, Char, Char)], [(Char, Char, Char)])] -> Int
isMASl = sum . map (\(l1, l2, l3) -> sum $ zipWith3 isMAS l1 l2 l3)

main :: IO ()
main = do contents <- getContents
          let ls = lines contents
          let enhanced = triples $ map triples ls
          let count = isMASl enhanced
          print count

