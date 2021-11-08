total' :: [[a]] -> Int
total' [] = 0
total' (x:xs) = length x + total' xs