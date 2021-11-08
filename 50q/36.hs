preCrescente :: Ord a => [a] -> [a] 
preCrescente [] = []
preCrescente (x:[]) = x:preCrescente []
preCrescente (x:xs) | x < head xs = x: preCrescente xs
					| otherwise = x:[]