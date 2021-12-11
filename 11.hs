import qualified Data.Map as M

fix f n x = let (x', n') = f x in if x == x' then (x, n) else fix f (n + n') x'

toMap s = M.fromList $ [((i, j), v) | 
    (i, r) <- zip [1..] $ map (read . (:[])) <$> lines s,
    (j, v) <- zip [1..] r]

step m = g $ fix flash 0 $ M.map (+1) m where 
    flash' a = M.mapWithKey (\k v -> if v > 9 then -100 else v + flashes a k) a
    flashes a = length . filter ((>9) . (a M.!)) . filter valid . neighbours
    neighbours (i, j) = [(i+x, j+y) | x <- [-1..1], y <- [-1..1]]
    valid (i, j) = elem i [1..10] && elem j [1..10]

    flash a = (flash' a, length . filter (>9) . M.elems $ a)
    g (a, b) = (M.map (max 0) a, b)

main1 = do 
    m <- readFile "11.txt" >>= pure . toMap

    print . snd . (\m -> foldl (\(m, n) _ -> 
        let (m',n') = step m in (M.map (max 0) m', n+n')) (m, 0) [1..100]) $ m
    
    print . fst . head . filter (all (==0). M.elems . snd) . zip [0..] $ iterate (fst.step) m