drop':: Int -> [a] -> [a]
drop' n [] = [] 
drop' n (x:xs) = if n == 0 then xs
						   else drop' (n-1) xs