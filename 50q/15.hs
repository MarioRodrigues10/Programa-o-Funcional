heads' :: [[a]] -> [a]
heads' [] = []
heads' ([]:t) = heads' t
heads' ((x:xs):ys) = if null (x:xs) then heads' ys else x: heads' ys 