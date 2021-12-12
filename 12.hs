import qualified Data.Map as M
import Data.Char ( isLower, isUpper ) 
import Data.List ( nub )

split s = words [if a == '-' then ' ' else a | a <- s]
toMap m x = M.insertWith (++) a [b] (M.insertWith (++) b [a] m) where [a, b] = x

paths ("end":xs) m = ["end":xs]
paths seen m =  concat [paths (a:seen) m 
    | a <- m M.! head seen , all isUpper a
    || elem a seen && a /= "start"  && (nub . filter (all isLower)) seen == filter (all isLower) seen -- remove this line for p1
    || notElem a seen] 

main = readFile "12/input.txt" >>= print . length . paths ["start"] . foldl toMap M.empty . map split . lines
    