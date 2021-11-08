removeLista:: Eq a => [a] -> [a]-> [a] 
removeLista [] _ = []
removeLista (x:xs) [] = (x:xs)
removeLista (x:xs) (y:ys) | x == y = removeLista xs ys
						  | otherwise = x:removeLista xs (y:ys)