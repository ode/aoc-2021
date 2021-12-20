import Data.Array

neighbours (i, j) = [(i + x, j + y) | x <- [-1 .. 1], y <- [-1 .. 1]]

getpixel img n pos
  | inRange (bounds img) pos = img ! pos
  | even n = '.'
  | otherwise = '#'

bin2dec = foldr (\c s -> s * 2 + c) 0 . reverse . map (fromEnum . (== '#'))

enhance algo n img = listArray newbounds [algo
    ! bin2dec (getpixel img n <$> neighbours pos)
    | pos <- range newbounds] where
    ((x1, y1), (x2, y2)) = bounds img
    newbounds = ((x1 - 2, y1 - 2), (x2 + 2, y2 + 2))

main = do
  (a : _ : xs) <- lines <$> readFile "20.txt"
  let algo = listArray (0, 511) a
      img = listArray ((0, 0), (length xs - 1, length xs - 1)) $ concat xs
  print . sum . map (fromEnum . (=='#')) . elems . enhance algo 1. enhance algo 0 $ img
  print . sum . map (fromEnum . (=='#')) . elems . foldr1 (.) (map (enhance algo) [49,48..0]) $ img