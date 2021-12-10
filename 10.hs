import Data.List (elemIndex, sort)
import Data.Map (fromList, (!))

close = (!) $ fromList [('(', ')'), ('[', ']'), ('{', '}'), ('<', '>')] 
score = (!) $ fromList [(')', 3), (']', 57), ('}', 1197), ('>', 25137)]
score' c = let Just s = elemIndex c "_)]}>" in s

parse (a : as) (x : xs)
  | x == close a = parse as xs
  | x `elem` "(<[{" = parse (x : a : as) xs
  | otherwise = Left x
parse as [] = Right $ map close as
parse [] (x : xs)
  | x `elem` "(<[{" = parse [x] xs
  | otherwise = Left x

main = do
  ls <- readFile "10.txt" >>= pure . lines
  let (lefts, rights) =
        foldl ( \(x, y) a -> case a of
              Left b -> (b : x, y)
              Right b -> (x, b : y)) ([], [])
          $ map (parse "") ls

  print $ sum $ map score lefts
  print $ sort (map (foldl (\n a -> n * 5 + score' a) 0) rights) !! div (length rights) 2