union :: Eq a => [a] -> [a]-> [a] 
union [] [] = []
union [] (x:xs) = (x:xs)
union (y:ys) [] = (y:ys)
union (y:ys) (x:xs) | elem x (y:ys) = union (y:ys) xs
				 	| otherwise = union ((y:ys) ++ [x]) xs


{-
union [1,1,2,3,4] [1,5]

union (1:[1,2,3,4]) (1:[5]) | elem 1 (1,1,2,3,4) = union [1,1,2,3,4] [5]
union (1:[1,2,3,4]) (5:[]) | otherwise = if 5 < 1 , else 1: union [1,2,3,4] (5:[])
union (1:[2,3,4]) (5:[]) | otherwise = if 5 < 1 , else 1: union [2,3,4] (5:[])
union (2:[3,4]) (5:[]) | otherwise = if 5 < 2 , else 2: union [3,4] (5:[])
union (3:[4]) (5:[]) | otherwise = if 5 < 3 , else 3: union [4] (5:[])
union (4:[]) (5:[]) | otherwise = if 5 < 4 , else 4: union [] (5:[])
union [] (5:[]) = (5:[])
1:1:2:3:4:5:[]

union [2,3,4] [1,5]
otherwise = union ([2,3,4] ++ [1]) [5]
union [2,3,4,1]

-}