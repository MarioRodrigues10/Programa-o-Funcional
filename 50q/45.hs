catMaybes :: [Maybe a] -> [a]
catMaybes [] = []
catMaybes (Nothing t) = catMaybes t
catMaybes (Just x:t) = x:catMaybes t