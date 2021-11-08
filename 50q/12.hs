concat':: [[a]] -> [a]
concat' [] = []
concat' (a:b) = a ++ concat' b


{-
 	a = [[1,2,3],[4,5,6],[7,8,9]]
	concat ([1,2,3]:[4,5,6],[7,8,9]) = [1,2,3] ++ concat [[4,5,6],[7,8,9]]
	++ = concatenar , junta 2 listas



 	-}