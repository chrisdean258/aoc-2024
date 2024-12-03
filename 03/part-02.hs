import Data.Char
import Data.List
import Data.Maybe
-- import Data.Ord
-- import Data.Bits
-- import Data.String
-- import qualified Data.Text as T

maybeRead :: (Read a) => String -> Maybe a
maybeRead (s:ss) | isDigit s = case reads (s:ss) of
                              [(x, "")] -> Just x
                              _ -> Nothing
                 | otherwise = Nothing


readInt :: String -> Char -> (Maybe Int, String)
readInt s sep = (v, if isJust v then r else s)
    where
        i = fromMaybe 0 $ elemIndex sep s 
        (l, _:r) = splitAt i s
        v = maybeRead l


results :: String -> Bool -> Int
results ('d':'o':'(':')':rest) active = results rest True 
results ('d':'o':'n':'\'':'t':'(':')':rest) active = results rest False 
results ('m':'u':'l':'(':rest) active = cont + if active then fromMaybe 0 ((*) <$> v1 <*> v2) else 0
    where
        (v1, rest1) = readInt rest ','
        (v2, rest2) = readInt rest1 ')'
        cont = results rest active
results (_:rest) active = results rest active
results s active = 0


main :: IO ()
main = do contents <- getContents
          print $ results contents True

