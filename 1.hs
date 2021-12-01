s a b c = a+b+c
zipped3 l = zipWith3 s l (tail l) (tail $ tail l)
zipped l = zip l (tail l)

prepare :: String -> [Int]
prepare = map read . lines

main = readFile "1.txt" >>=  print . length .filter (\(a,b) -> a < b). zipped . zipped3 . prepare