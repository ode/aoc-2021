(xmin, xmax) = (230, 283)
(ymin, ymax) = (-107, -57)
f x = x * (x-1) `div` 2
trajectory vx vy = takeWhile ((>=ymin) . snd) 
  [ (if t < vx then x else f (vx+1), y) 
  | t <- [1..]
  , let x = vx * t - f t
  , let y = vy * t - f t
  ]
valid = any (\(x, y) -> xmin <= x && x <= xmax && ymin <= y && y <= ymax)
allTrajectories = filter valid [trajectory vx vy | vx <- [1..xmax], vy <- [ymin.. -ymin]]

main = do
    print $ maximum . map snd . concat $ allTrajectories
    print $ length allTrajectories