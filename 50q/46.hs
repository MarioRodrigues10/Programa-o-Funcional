data Movimento = Norte | Sul | Este | Oeste
		deriving Show

caminho :: (Int,Int) -> (Int,Int) -> [Movimento] 
caminho (a,b) (c,d) | a < c = Norte: caminho (a,b) (c-1,d)
					| a > c = Sul : caminho (a,b) (c+1,d)
					| b < d = Este: caminho (a,b) (c,d-1)
					| b > d = Oeste: caminho (a,b) (c,d+1)
					| otherwise = []


{- 
	(1,1) (3,3)
	1 < 3 
-} 