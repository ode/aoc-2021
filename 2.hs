data Instruction = Forward Int | Down Int | Up Int

parseIn :: String -> Instruction
parseIn ('f':xs) = Forward (read . last. words $ xs)
parseIn ('u':xs) = Up (read . last. words $ xs)
parseIn ('d':xs) = Down (read . last. words $ xs)

run1 x depth (Forward a:xs) = run1 (x+a) depth xs
run1 x depth (Up a:xs) = run1 x (depth-a) xs
run1 x depth (Down a:xs) = run1 x (depth+a) xs
run1 x depth [] = (x, depth )

run2 x depth aim (Forward a:xs) = run2 (x+a) (depth+(aim*a)) aim xs 
run2 x depth aim (Up a:xs) = run2 x depth (aim-a) xs 
run2 x depth aim (Down a:xs) = run2 x depth (aim+a) xs 
run2 x depth aim [] = (x, depth)

main = readFile "2.txt" >>= print.(\(a,b) -> a*b). run2 0 0 0 . map parseIn . lines
-- replace run2 0 0 0 with run1 0 0 for p1
