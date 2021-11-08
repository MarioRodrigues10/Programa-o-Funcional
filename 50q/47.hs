data Movimento = Norte | Sul | Este | Oeste deriving Show

hasLoops :: (Int,Int) -> [Movimento] -> Bool 
hasLoops (a,b) [] = False
hasLoops (a,b) x | elem (a,b) (aux (a,b) x) = True
				 | otherwise = False

aux:: (Int,Int) -> [Movimento] -> [(Int,Int)]
aux (a,b) [] = []
aux (a,b) (x:xs) = case x of 
  Norte -> (a,b+1):aux (a,b+1) xs
  Sul -> (a,b-1):aux (a,b-1) xs
  Este -> (a+1,b):aux (a+1,b) xs
  Oeste -> (a-1,b):aux (a-1,b) xs
