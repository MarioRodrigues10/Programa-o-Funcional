isPrime:: Int -> Bool
isPrime x | x >= 2 = testar' x 2
		  | otherwise = False

testar' n m | m*m > n = True
			| mod n m == 0 = False
			| otherwise = testar' n (m+1)