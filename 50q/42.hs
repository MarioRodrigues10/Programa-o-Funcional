removeMSet:: Eq a => a -> [(a,Int)] -> [(a,Int)]
removeMSet _ [] = []
removeMSet letra ((a,b):xs) | a == letra = removeMSet letra xs
							| otherwise = (a,b):removeMSet letra xs