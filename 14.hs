import qualified Data.Map as M
import Data.List (sortOn)

toTuple rule = ([a,b], [[a,c],[c,b]]) where [[a,b], _, [c]] = words rule

window2 t = zipWith (\a b-> [a, b]) t (tail t)

step ruleMap freqMap = foldr1 (.) (concat [
     M.insertWith (flip (-)) a (freqMap M.! a)
    : map (\b -> M.insertWith (+) b (freqMap M.! a)) (ruleMap M.! a)
    | a <- M.keys freqMap]) freqMap

main = do
    (template':_:rules') <- readFile "14.txt" >>= pure . lines
    let
        rules = M.fromList $ map toTuple rules'
        freqMap = foldl (\m a -> M.insertWith (+) a 1 m) M.empty $ window2 template'
        finMap = iterate (step rules) freqMap !! 40 -- 10 for p1
        x = sortOn snd . M.toList $ M.foldrWithKey (\(_:[a]) v m -> M.insertWith (+) a v m) M.empty finMap
    print $ snd (last x) - snd (head x)