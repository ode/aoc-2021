import Data.List ( sort )

median l = sort l !! (length l `div` 2)
mean l = sum l `div` length l 

optimise1 l = sum [abs (median l-i) | i <- l] :: Int

calc n l = sum [div (f i^2 + f i) 2 | i <- l] where f = abs . (n-)
optimise2 l = minimum [calc n l | n <- [median l..mean l]]

main = readFile "7.txt" >>= print . optimise2 . read . (\s -> "[" ++ s ++ "]") -- optimise1 for p1