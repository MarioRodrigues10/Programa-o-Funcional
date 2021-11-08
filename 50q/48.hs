type Ponto = (Float,Float)
data Rectangulo = Rect Ponto Ponto

contaQuadrados :: [Rectangulo] -> Int 
contaQuadrados [] = 0
contaQuadrados ((Rect (a,b)(c,d)):xs) | abs (a-c) == abs (b-d) = 1+ contaQuadrados xs
							  		  | otherwise = contaQuadrados xs


{- 
 (a,b) (c,d) = abs(a-c) == abs(b-d)
  

  
-}