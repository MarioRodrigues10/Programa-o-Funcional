delete :: Eq a => a -> [a] -> [a]
delete x [] = []
delete x (y:ys) | x == y = ys
				| otherwise = y:delete x ys
