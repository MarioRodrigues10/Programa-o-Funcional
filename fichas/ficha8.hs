data Frac = F Integer Integer

instance Eq Frac where
	(F a b) (F c d) = a*d == b*c

instance Ord Frac where
	(F a b) <= (F c d) = a*d <= c*d

instance Show Frac where
	(F a b) = show a ++ "/" ++ show b 

instance Num Frac where
{-
Classe Num, precisa:
 + , * , - :: a -> a -> a
 negativo(negate), abs(modulo), signum (números negativos retorna -1 , número neutro retorna 0, números positivos retorna 1)
 from integer -> int

-}
	(F a b) + (F c d) | b == d = normaliza (F (a+c) b)
					  | otherwise = normaliza (F (a*d + b*c) (b*d))
	x - y = x + negate y
	abs (F a b) = F (abs a) (abs b)
	signum (F a b) | a == 0 = 0
				   | a * b > 0 = 1
				   | otherwise = -1
	fromInteger x = F x 1
	