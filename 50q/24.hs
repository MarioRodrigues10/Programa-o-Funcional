isSubsequenceOf:: Eq a => [a] -> [a] -> Bool
isSubsequenceOf [] [] = True
isSubsequenceOf [] _ = False
isSubsequenceOf _ [] = False
isSubsequenceOf (x:xs) (y:ys)   | x == y  = isSubsequenceOf xs ys
							 	| x /= y  = isSubsequenceOf (x:xs) ys