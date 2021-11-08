intersect :: Eq a => [a] ->[a] -> [a] 
intersect [] [] = []
intersect [] _ = []
intersect _ [] = []
intersect (x:xs) (y:ys) | elem x (y:ys) = x:intersect xs (y:ys)
						| otherwise = intersect xs (y:ys)