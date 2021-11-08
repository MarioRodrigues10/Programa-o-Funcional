intersperse' :: a -> [a] -> [a]
intersperse' _ [] = []
intersperse' _ [p] = [p]
intersperse' x (y:ys) = y:x:intersperse' x ys