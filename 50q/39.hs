elemMSet :: Eq a => a -> [(a,Int)] -> Bool
elemMSet x [] = False
elemMSet x ((a,b):xs) | x == a = True
					  | otherwise = elemMSet x xs