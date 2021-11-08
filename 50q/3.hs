(++) :: [a] -> [a] -> [a]
(++) [] l = l
(++) (h:t) l = h : (++) t l

{- 
	[1,2,3] [4,5,6]

	(1:[2,3]) [4,5,6] = 1 : (++) [2,3] [4,5,6]

	(2:[3]) [4,5,6] = 2 : (++) [3] [4,5,6]

	(3:[]) [4,5,6] = 3 : [] [4,5,6]

	[] [4,5,6] = [4,5,6]

	Output : [1,2,3,4,5,6]

Começamos por percorrer os elementos todos da 1ª lista, assim que esta percorremos todos elementos da
mesma ela vai ficar vazia. E vamos á nossa segunda linha . [] [4,5,6] = [4,5,6]


-}