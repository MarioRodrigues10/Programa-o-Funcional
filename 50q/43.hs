constroiMSet :: Ord a => [a] -> [(a,Int)] 
constroiMSet [] = []
constroiMSet (x:xs) = insereMSet x (constroiMSet xs)

insereMSet :: Eq a => a -> [(a,Int)] -> [(a,Int)]
insereMSet x [] = [(x,1)]
insereMSet x ((a,b):xs) | x == a = (a,b+1):xs
						| otherwise = (a,b):insereMSet x xs
