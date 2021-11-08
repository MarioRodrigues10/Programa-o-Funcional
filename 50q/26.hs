nub:: Eq a => [a] -> [a]
nub [] = []
nub (x:xs) | elem x xs = nub (xs) 
		   | otherwise = x:nub (xs)

{-  
 nub [1,2,1,2,3,1,2] 
	elem  1 [2,3,4,5,6] = false
	elem 1 [1,2,3,4] = true
-}