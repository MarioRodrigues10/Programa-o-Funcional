elemIndices:: Eq a => a -> [a] -> [Int]
elemIndices x l = eIA 0 x l

eIA:: Eq a => Int -> a -> [a] -> [Int]
eIA p x [] = []
eIA p x (h:t) | x == h = p: eIA p+1 x t
			  | otherwise = eIA (p+1) x t