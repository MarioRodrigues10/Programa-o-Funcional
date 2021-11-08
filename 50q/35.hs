 lookup' :: Eq a => a -> [(a,b)] -> Maybe b
 lookup' _ [] = Nothing
 lookup' x ((a,b):ys) | x == a = Just b
                      | otherwise = lookup' x ys

{-  Cuidado a copiar a  solução, porque as aspas vão dar erros na CMD-}