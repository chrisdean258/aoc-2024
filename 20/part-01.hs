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

type Point = (Int, Int)


index2 :: [[a]] -> [((Int, Int), a)]
index2 a = concatMap (\(i, v) -> zipWith ((,).(i,)) [0..] v) (zip [0..] a)

surround :: S.Set Point -> Point -> [Point]
surround pts (x, y) = filter (`S.member` pts) [(x + 1, y), (x - 1, y), (x, y+1), (x, y-1)]


bfs :: S.Set Point -> M.Map Point Int -> M.Map (Int, Int) Point -> Int -> M.Map Point Int
bfs pts seen q unique
 | null q = seen
 | otherwise = bfs pts seen' q'' (unique + 5)
    where
        (((c, _), p), q') = M.deleteFindMin q
        seen' = M.insert p c seen
        downstream = filter (`M.notMember` seen') $ surround pts p
        q'' = foldl (\qq (p, u) -> M.insert (c + 1, unique + u) p qq) q' $ zip downstream [1..] 
    


shortestPaths :: S.Set Point -> M.Map (Point, Point) Int
shortestPaths pts = dists
    where
        as_list = S.toList pts
        dists = M.fromList $ concatMap (\p -> map (\(to, c) -> ((p, to), c)) $ M.toList $ bfs pts M.empty (M.singleton (0,0) p) 1) as_list

cheats :: S.Set Point -> [Point] -> [(Point, Point)]
cheats _ [] = []
cheats pts (w:ws) = [(p1, p2) | p1 <- s, p2 <- s] ++ cheats pts ws
    where
        s = surround pts w

mLookup :: Ord a => a -> M.Map a b -> b
mLookup a = fromJust . M.lookup a

savedD :: M.Map (Point, Point) Int -> Point -> Point -> (Point, Point) -> Int
savedD apd start end (cs, ce) = d1 - d2
    where
        d1 = mLookup (start, end) apd
        d2 = mLookup (start, cs) apd + mLookup (ce, end) apd + 2

main :: IO ()
main = do contents <- getContents
          let (boardl, wallsl) = partition ((/='#').snd) $ index2 $ lines contents
          let walls = map fst wallsl
          let start = head $ map fst $ filter ((=='S').snd) boardl
          let end = head $ map fst $ filter ((=='E').snd) boardl
          let board = S.fromList $ map fst boardl
          let fw = shortestPaths board
          let cts = cheats board walls
          let saved :: [Int] = map (savedD fw start end) cts
          let cond = length $ filter (>=100) saved
          -- print $ map (\l -> (head l, length l)) $ group $ sort saved
          print cond

