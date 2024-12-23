import Debug.Trace
import Data.Function
import Data.List
import Data.Maybe
import Data.Char
import Data.Ord
import Data.Bits
import Data.String
import Data.Tuple
import qualified Data.Text as T
import qualified Data.Map as M
import qualified Data.Set as S

pairs :: Show a => [a] -> [(a, a)]
pairs (a:b:rest) = (a,b):pairs rest
pairs _ = []

makeCircle :: [String] -> S.Set (String, String) -> (String, String) -> [[String]]
makeCircle nodes edges (n1, n2) = success
    where
        makeCircle1 n3 = (n3, n1) `S.member` edges && (n3, n2) `S.member` edges
        circles = map makeCircle1 nodes
        success = map (sort.(:[n1, n2]).snd) $ filter fst $ zip circles nodes

main :: IO ()
main = do contents <- getContents
          let ls = lines contents
          let nodes = concatMap (\v -> map ($ v) [take 2, drop 3]) ls
          let sorted = map head $ group $ sort nodes
          let edges = pairs nodes
          let edge_set = S.fromList (edges ++ map swap edges)
          let t_edges = filter (\(a, b) -> head a == 't' || head b == 't') edges
          let t_nodes = filter ((== 't').head) sorted
          let circles = S.fromList $ concatMap (makeCircle t_nodes edge_set) edges
          print $ S.size circles

