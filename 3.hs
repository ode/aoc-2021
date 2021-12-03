import Data.List

g s = map (\x -> (length x, head x)) . group . sort $ s

bin2dec = foldr (\c s -> s * 2 + c) 0 . reverse . map c2i
    where c2i c = if c == '0' then 0 else 1

f '0' = '1'
f '1' = '0'

oxygen (x:[]) = [x]
oxygen l = map (a:) (oxygen $ map tail $ filter ((==a).head)  l)
    where a = (snd . last . sort . g $ map head l)

co2 (x:[]) = [x]
co2 l = map (a:) (co2 $ map tail $ filter ((==a).head)  l)
    where a = (snd . last . reverse . sort . g $ map head l)

--main = readFile "3.txt" >>=  print .uncurry (*). (\s -> (bin2dec s, bin2dec $ map f s)). map (snd . head . sort . g) . transpose . lines
main = do
    l <- readFile "3.txt" >>= pure . sort . lines
    print $ product $ map (bin2dec . head) [oxygen l, co2 l]
    
