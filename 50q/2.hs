enum:: Int -> Int -> Int -> [Int]
enum x y z 
			| (x > y && y > z) || (x == y && y == z) || x > z = []
			| otherwise = x:enum y (y+y-x) z
				




