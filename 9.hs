import Data.List (transpose, sortOn)
import Data.Map (fromList, (!), adjust)

lowpoints l = sum $ zipWith (\(a,b) (c,d) -> if a&&c then b+1 else 0) 
    (concatMap g l) $
    concat . transpose . map g $ transpose l
    where
        f (x:y:z:xs) = (y < z && y < x, y) : f (y:z:xs)
        f _ = []
        g l = f $ [9] ++ l ++ [9]

valid (a,b) = 1 <= a && a <= 100 && 1 <= b && b <= 100
bfs [] n m = (n, m)
bfs (q:qs) n m
    | snd (m ! q) || fst (m ! q) == 9 =  bfs qs n m
    | otherwise = bfs (qs ++ neighbours) (n + 1) (adjust (\(n,_) -> (n,True)) q m) where
        neighbours = filter valid [(x + 1, y), (x - 1, y), (x, y + 1), (x, y - 1)]
        (x, y) = q

--main = readFile "9.txt" >>= print . lowpoints . map (map (read . (:[]))) . lines
main = do
    m <- readFile "9.txt" >>= pure . map (map (read . (:[]))). lines
    let m' = fromList [((i, j), (a, False)) | (i, b) <- zip [1..] m, (j,a) <- zip [1..] b]
    
    print $ product . take 3 . sortOn negate . fst $ 
        foldl (\(n, m) q -> let (n', m') = bfs [q] 0 m in (n':n, m')) ([], m')
        [(i, j) | i <- [1..100], j <- [1..100]]