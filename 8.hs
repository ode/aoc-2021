import Data.Set ( fromList, isSubsetOf, (\\), union, size, intersection )
import Data.List ( elemIndex, sortOn, find )

parse = (\l -> (take 10 l, drop 11 l)) . words

deduce (inp, target) = concatMap show <$> mapM (`elemIndex` [s0,s1,s2,s3,s4,s5,s6,s7,s8,s9]) (fromList <$> target) where
    sets = fromList <$> sortOn length inp
    [s1, s7, s4, _, _, _, _, _, _, s8] = sets
    Just s5 = find (\s -> size s == 5 && (s4 \\ s1) `isSubsetOf` s) sets
    s9 = union s4 s5
    [s2, s3, _] = sortOn (size . intersection s5) $ filter ((==5) . size) sets
    [s6, s0] = sortOn (size . intersection s1) $ filter (\s -> s /= s9 && size s == 6) sets

main = readFile "8.txt" >>= print . fmap (sum . map read). mapM (deduce . parse) . lines
-- main = readFile "8.txt" >>= print . length . filter (\n -> n `elem` [2,3,4,7]). concatMap (map length . drop 11 . words) . lines
