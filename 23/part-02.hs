import Debug.Trace
import Data.Function
import Data.List
import Data.Maybe
import Data.Char
import Data.Ord
import Data.Bits
import Data.String
import Data.Tuple
import qualified Data.Set as Set

import qualified Data.IntSet as S
import qualified Data.Vector as V

-- | Given a list of nodes, and a function that determines whether there is an edge between any two nodes, yields a list of maximal cliques -- sets of nodes such that every node is connected to every other, and such that no other node may be added while maintaining this property.
getMaximalCliques :: (a -> a -> Bool) -> [a] -> [[a]]
getMaximalCliques tolFun xs = map (map (fst . (V.!) lv) . S.toList) $
                              maximalCliques pickpivot (snd . (V.!) lv) (S.fromList $ map fst lnodes)
    where lnodes = zip [0..] xs
          lnodes' = map (\(k,n) -> (n,S.fromList $ filter (/=k) $ map fst $ filter (tolFun n . snd) lnodes)) lnodes
          lv = V.fromList lnodes'
          pickpivot p x = head $ S.elems p ++ S.elems x

-- | The Bron-Kerbosch algorithm for finding all maximal cliques in an undirected graph.
-- <http://en.wikipedia.org/wiki/Bron%E2%80%93Kerbosch_algorithm>. Works on nodes represented as 'Int's.
maximalCliques :: (S.IntSet -> S.IntSet -> Int) -- ^ A function that given two 'IntSet's, chooses a member of one as a pivot.
               -> (Int -> S.IntSet)  -- ^ A function that given a node id, yields the set of its neighbors.
               -> S.IntSet -- ^ The set of all nodes in the graph.
               -> [S.IntSet] -- ^ An enumeration of all maximal cliques in the graph.
maximalCliques pickpivot neighborsOf nodeset = go S.empty nodeset S.empty
    where go r p x
              | S.null p && S.null x = [r]
              | otherwise =
                  let pivot = pickpivot p x
                      step' (p',x') v =
                          let nv  = neighborsOf v
                          in ((S.delete v p', S.insert v x'), go (S.insert v r) (S.intersection nv p') (S.intersection nv x'))
                  in concat . snd $ mapAccumL step' (p,x) $ S.elems (p S.\\ neighborsOf pivot)


pairs :: Show a => [a] -> [(a, a)]
pairs (a:b:rest) = (a,b):pairs rest
pairs _ = []

main :: IO ()
main = do contents <- getContents
          let ls = lines contents
          let nodes = concatMap (\v -> map ($ v) [take 2, drop 3]) ls
          let sorted = map head $ group $ sort nodes
          let edges = pairs nodes
          let edge_set = Set.fromList (edges ++ map swap edges)
          let mc = getMaximalCliques (curry (`Set.member` edge_set)) sorted
          print $ tail $ concatMap (',':) $ maximumBy (compare `on` length) $ map sort mc

