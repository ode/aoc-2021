import Text.ParserCombinators.ReadP

data Pair = Int Int | Pair Pair Pair deriving (Eq, Show)

pairParser = do
  a <- get
  if a == '['
    then do
      p1 <- pairParser
      char ','
      p2 <- pairParser
      char ']'
      return $ Pair p1 p2
    else do
      pure . Int . read $ [a]

parse = fst . last . readP_to_S pairParser

fix f x = if x == f x then x else fix f (f x)

split (Int a) = if a > 9 then Pair (Int (a `div` 2)) (Int (a - (a `div` 2))) else Int a
split (Pair a b) = if split a == a then Pair a (split b) else Pair (split a) b

explode _ (Int a) = (Int a, Nothing)
explode 4 (Pair (Int a) (Int b)) = (Int 0, Just (a, b))
explode n (Pair a b) = case explode (n + 1) a of
  (a', Just (l, r)) -> (Pair a' $ add' True r b, Just (l, 0))
  (a', Nothing) -> case explode (n + 1) b of
    (b', Just (l, r)) -> (Pair (add' False l a) b', Just (0, r))
    (b', Nothing) -> (Pair a' b', Nothing)
  where
    add' bool n (Pair a b) = if bool then Pair (add' bool n a) b else Pair a (add' bool n b)
    add' _ n (Int n') = Int $ n + n'

reduce = fix (split . fix (fst . explode 0))

add p1 p2 = reduce $ Pair p1 p2

magnitude (Pair a b) = 3 * magnitude a + 2 * magnitude b
magnitude (Int a) = a

main = do
  terms <- readFile "18.txt" >>= pure . map parse . lines
  print . magnitude . foldl1 add $ terms
  print . maximum . map (magnitude . uncurry add) $ [(a, b) | a <- terms, b <- terms, a /= b]