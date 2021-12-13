import qualified Data.Map as M
import Data.Maybe (fromMaybe)
import Data.List (unfoldr, transpose)

look m x = fromMaybe False $ M.lookup x m
foldx m x = M.mapWithKey (\(x', y') b -> b || look m (2*x - x', y')) $ M.filterWithKey (\k _ -> snd k < x) m
foldy m y = M.mapWithKey (\(x', y') b -> b || look m (x', 2*y - y')) $ M.filterWithKey (\k _ -> snd k < y) m
fold m (a, b) = if a then foldx m b else foldy m b

chunks n = takeWhile (not . null) . unfoldr (Just . splitAt n)
prettyprint m = unlines . transpose . chunks x . map ((\a -> if a then '#' else '.') . snd) $ M.toList m where 
    x = length . filter ((==0) . fst . fst) $ M.toList m

main = do
    (c', _:f') <- readFile "13.txt"  >>= pure . span (',' `elem`). lines

    let
        c = map (\s -> read $ "(" ++ s ++ ")") c'
        (x, y) = (maximum $ map fst c, maximum $ map snd c)

        m = foldl (\m' c' -> M.insertWith (||) c' False m') (M.fromList $ [(pos, True) | pos <- c])
            [(i, j) | i <- [0..x], j <- [0..y]]

        f = map (\s -> case head . drop 2 $ words s of
            'x':'=':xs -> (True, read xs)
            'y':'=':xs -> (False, read xs)) f'

    print . length . filter id . M.elems . fold m $ head f
    putStr . prettyprint $ foldl fold m f