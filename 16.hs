import Text.ParserCombinators.ReadP
import Numeric (readHex)

data Packet = Literal Int Int | Operator Int Int [Packet]

bin2dec = foldr (\c s -> s * 2 + c) 0 . reverse . map (fromEnum . (=='1'))
dec2bin n = if n > 0 then dec2bin (div n 2) ++ show (mod n 2) else []
f s = if mod l 4 == 0 then s else replicate (4 - mod l 4) '0' ++ s where l = length s

bit = choice [char '0', char '1']
bits n = count n bit >>= pure . bin2dec
packet = do
  version <- bits 3
  typeId <- bits 3
  if typeId == 4 then do
    s1 <- manyTill (count 5 bit) (char '0')
    s2 <- count 4 bit
    return $ Literal version (bin2dec . concat $ tail <$> s1 ++ [s2])
  else do
    lengthType <- bit
    if lengthType == '0' then do
      n <- bits 15
      packetstring <- count n bit
      let packets = fst . last $ readP_to_S (many1 packet) packetstring
      return $ Operator version typeId packets
    else do
      n <- bits 11
      packets <- count n packet
      return $ Operator version typeId packets

versionSum (Literal a _) = a
versionSum (Operator a _ xs) = a + sum (versionSum <$> xs)

value (Literal _ i) = i
value (Operator _ i l') = let l = value <$> l' in case i of
  0 -> sum l
  1 -> product l
  2 -> minimum l
  3 -> maximum l
  5 -> fromEnum (l !! 0 > l !! 1)
  6 -> fromEnum (l !! 0 < l !! 1)
  7 -> fromEnum (l !! 0 == l !! 1)

main = do
    p <- readFile "16.txt" >>= pure . fst . last . readP_to_S packet . f . dec2bin . fst . head . readHex
    print $ versionSum p
    print $ value p