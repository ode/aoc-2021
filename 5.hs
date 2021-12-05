{-# LANGUAGE NamedFieldPuns #-}
import Data.Array.MArray hiding ( range )
import Control.Monad ( forM_ ) 
import Data.Array.IO ( IOArray )

data Vent = Vent { 
    x1 :: Int, y1 :: Int,
    x2 :: Int, y2 :: Int
} -- sussy baka

parseVent s = Vent {x1, y1, x2, y2} where
    [p1,_,p2] = words s
    (x1,y1) = read $ "(" ++ p1 ++ ")"
    (x2,y2) = read $ "(" ++ p2 ++ ")"

horiz v = y1 v == y2 v && x1 v /= x2 v
vert v = x1 v == x2 v && y1 v /= y2 v
st a b 
    | a < b = [a..b]
    | a == b = [a,a..]
    | otherwise =  [a,a-1..b]
range v = zip (st (x1 v) (x2 v)) (st (y1 v) (y2 v))

ventMatrix :: [Vent] -> IO Int
ventMatrix vs = do -- uncomment the if ternary for p1
    arr <- newArray ((0, 0), (1000, 1000)) 0 :: IO (IOArray (Int, Int) Int)
    forM_ vs (\v -> do
        forM_ (range v) (\pos-> do
            val <- readArray arr pos
--          if (horiz v || vert v) then
            writeArray arr pos (val + 1) 
--          else return ()
            )
        )
    l <- getElems arr
    return $ sum $ map (\n -> if n > 1 then 1 else 0) l

main = readFile "5.txt" >>=  ventMatrix . map parseVent . lines >>= print