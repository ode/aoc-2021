{-# LANGUAGE TupleSections #-}
import Data.List ( transpose )
import Control.Monad ( forM )

readBoard = getLine >> forM [1..5] (const $ map ((False,) . read) . words <$> getLine)

checkBoard b = or [all fst l | l <- b ++ transpose b]

call n b = [[if snd a == n then (True, snd a) else a | a <- r] | r <- b]

score n b = (n*) $ sum [sum $ map snd . filter (not.fst) $ r | r <- b]

f (n:ns) bs = 
    if or . map checkBoard $ nbs then
        score n $ head . filter checkBoard $ nbs
    else f ns nbs
    where nbs = call n <$> bs

g (n:_) [b] = score n $ call n b
g (n:ns) bs = g ns $ filter (not.checkBoard) $ call n <$> bs

main = do
    s <- getLine
    let ns = read ("[" ++ s ++ "]")
    bs <- forM [1..100] (const readBoard)
    print $ g ns bs -- this is f ns bs for p1