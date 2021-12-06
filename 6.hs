import Data.List

-- naive implementation, works for p1
simulate 0 l = length l
simulate n l = simulate (n-1) $ concatMap f l
    where f a = if a == 0 then [6,8] else [a-1]

fib9  = replicate 9 1 ++ zipWith (+) fib9 (drop 2 fib9)
simulate' n = sum . map (\a -> f (head a) * length a) . group . sort
    where f a = fib9 !! (n + 8-a)

main = readFile "6.txt" >>= print . simulate' 256 . read . (\s -> "[" ++ s ++ "]")