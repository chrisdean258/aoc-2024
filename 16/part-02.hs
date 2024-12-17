import Data.Function
import Data.List
import Data.Maybe
import qualified Data.Map as M
import qualified Data.Set as S

type Point = (Int, Int)
type Direction = (Int, Int)
type Heading = (Point, Direction)
type Queue = M.Map (Int, Int) Heading
type Set = S.Set Heading

directions :: [Direction]
directions = [(0,1), (1, 0), (0,-1), (-1,0)]

rotateDir :: Direction -> Direction
rotateDir (ud, lr) = (-lr, ud)

rotateDir' :: Direction -> Direction
rotateDir' (ud, lr) = (lr, -ud)

(+++) :: Point -> Direction -> Point
(+++) (px, py) (dx, dy) = (px + dx, py + dy)

index2 :: [[a]] -> [((Int, Int), a)]
index2 a = concatMap (\(i, v) -> zipWith ((,).(i,)) [0..] v) (zip [0..] a)

update :: Heading -> Set -> Queue -> (Int, Int) -> M.Map Heading (Int, [Heading]) -> Heading -> (Queue, M.Map Heading (Int, [Heading]))
update h all_points q (c, unique) seen_pts parent
 | h `S.notMember` all_points = (q, seen_pts)
 | isNothing val = (enqueue, insert_first)
 | cost == c = (q, add_parent)
 | otherwise = (q, seen_pts)
    where
        val = M.lookup h seen_pts 
        (cost, parents) = fromJust val
        enqueue = M.insert (c, unique) h q
        add_parent = M.insert h (cost, parent:parents) seen_pts
        insert_first = M.insert h (c, [parent]) seen_pts

solve :: Set -> Point -> Queue -> Int -> M.Map Heading (Int, [Heading]) -> M.Map Heading (Int, [Heading])
solve points target q c seen
 | null q = seen
 | otherwise = solve points target q'' (c + 3) seen'''
    where
        (((v, _), (p, d)), q') = M.deleteFindMin q
        (s_q, seen') = update (p +++ d, d) points q' (v + 1, c + 1) seen (p, d)
        (r_q, seen'') = update (p, rotateDir d) points s_q (v + 1000, c + 2) seen' (p, d)
        (q'', seen''') = update (p, rotateDir' d) points r_q (v + 1000, c + 3) seen'' (p, d)
        
collect :: [Heading] -> M.Map Heading (Int, [Heading]) -> S.Set Heading -> S.Set Heading
collect [] m acc = acc
collect (h:hs) m acc = collect (additional ++ hs) m acc'
    where
        acc' = S.insert h acc
        additional = M.lookup h m & fromJust & snd

main :: IO ()
main = do contents <- getContents
          let map_ = index2 $ lines contents
          let map_points :: Set = map_ & filter (\c -> snd c /= '#') & concatMap (\v -> map (fst v,) directions) & S.fromList
          let (start, _) = map_ & filter (\c -> snd c == 'S') & head
          let (target, _) = map_ & filter (\c -> snd c == 'E') & head
          let soln = solve map_points target (M.singleton (0,0) (start, (0, 1))) 0 $ M.singleton (start, (0, 1)) (0, [])
          let starts = filter (\h -> (fst . fst) h == target) $ M.toList soln
          let true_starts = map (fst.snd) $ head $ groupBy (\a b -> fst a == fst b) ( sort $ zip (map (fst . snd) starts) starts) 
          let all_headings = group $ sort $ map fst $ S.toList $ collect true_starts soln S.empty
          print $ length all_headings
