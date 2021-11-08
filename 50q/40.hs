replicate' :: Int -> a ->[a]
replicate' 0 _ = []
replicate' x y  | x > 0 = y:replicate' (x-1) y

converteMSet :: [(a,Int)] -> [a]
converteMSet [] = []
converteMSet ((a,b):xs) | b > 0 = (replicate' b a) ++ converteMSet xs
						| otherwise = converteMSet xs