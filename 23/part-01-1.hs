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
import Data.Map ((!))
import qualified Data.Set as S
import Control.Monad
import Control.Monad.ST
import Data.Array.MArray
import Data.Array.ST
import Data.STRef
import Prelude hiding (id)
import Data.Bifunctor

-- Union Find from https://gist.github.com/kseo/8693028
data UnionFind s = UnionFind {
    ids:: STUArray s Int Int
  , szs:: STUArray s Int Int
  }

newUnionFind :: Int -> ST s (UnionFind s)
newUnionFind n = liftM2 UnionFind (newListArray (0, n-1) [0..n-1]) (newArray (0, n-1) 1)

findU :: UnionFind s -> Int -> Int -> ST s Bool
findU uf p q = liftM2 (==) (root uf p) (root uf q)

root :: UnionFind s -> Int -> ST s Int
root uf i = do
    id <- readArray (ids uf) i
    if id /= i
        then do
            gpid <- readArray (ids uf) id
            writeArray (ids uf) i gpid
            root uf id
        else return i

unite :: UnionFind s -> Int -> Int -> ST s ()
unite uf p q = do
    i <- root uf p
    j <- root uf q
    szi <- traceShow (i, j) $ readArray (szs uf) i
    szj <- readArray (szs uf) j
    if szi < szj
        then do
            writeArray (ids uf) i j
            writeArray (szs uf) j (szi + szj)
        else do
            writeArray (ids uf) j i
            writeArray (szs uf) i (szj + szi)

pairs :: Show a => [a] -> [(a, a)]
pairs (a:b:rest) = (a,b):pairs rest
pairs _ = []

main :: IO ()
main = do contents <- getContents
          let ls = lines contents
          let all = concatMap (\v -> map ($ v) [take 2, drop 3]) ls
          let pairs' = pairs all
          let unique = map head $ group $ sort all
          print unique
          let mapping = M.fromList $ zip unique [0..]
          let group_nums = runST $ do {
            uf <- newUnionFind (M.size mapping)
            ; mapM_ (uncurry (unite uf) . bimap (mapping !) (mapping !)) pairs'
            ; mapM (root uf . (mapping !)) unique
          }
          -- let sorted = map (map fst) $ groupBy ((==) `on` snd) $ sortBy (compare `on` snd) node_to_group
          print group_nums

