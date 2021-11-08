type Ponto = (Float,Float)
data Rectangulo = Rect Ponto Ponto

areaTotal :: [Rectangulo] -> Float
areaTotal [] = 0
areaTotal ((Rect (a,b)(c,d)):xs) = abs(a-c)*abs(b-d) + areaTotal xs

