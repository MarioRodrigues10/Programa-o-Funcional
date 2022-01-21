type Monomio = (Float,Int)
type Polinomio = [Monomio]




any':: (a -> Bool) -> [a] -> Bool
any' f [] = False
any' f (h:t) = f h || any' f t

{-
função f (a -> Bool) 
 função f aplicada à head 
 recursividade da função f aplicada à tail
-}

zipWith':: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith' f (x:xs) (y:ys) = f x y: zipWith' f xs ys
zipWith' f _ _= []

takeWhile':: (a -> Bool) -> [a] -> [a]
takeWhile' f [] = []
takeWhile' f (x:xs) | f x = x: takeWhile' f xs
					| otherwise = []


dropWhile' :: (a -> Bool) -> [a] -> [a]
dropWhile' f [] = []
dropWhile' f (x:xs) | f x = dropWhile' f xs
					| otherwise = xs

-- H
span' :: (a -> Bool) -> [a] -> ([a],[a])
span' f [] = ([],[])
span' f l = (takeWhile' f l, dropWhile' f l)



deleteBy :: (a -> a -> Bool) -> a -> [a] -> [a]
deleteBy f _ [] = []
deleteBy f a (x:xs) | f a x = xs
					| otherwise = deleteBy f a xs 

-- SortOn Help

--2 

selgrau :: Int -> Polinomio -> Polinomio
selgrau _ [] = []
selgrau x ((a,b):t) | x == b = (a,b): selgrau x t
					| otherwise = selgrau x t

conta :: Int -> Polinomio -> Int
conta _ [] = 0
conta x ((a,b):t) | x == b = 1 + conta x t
			  	  | otherwise = conta x t

{-deriv :: Polinomio -> Polinomio
deriv [] = []
deriv ((a,b):t) | b > 0 = (fromIntegral a * (b),b-1): deriv t
				| b == 0 = deriv t
				| otherwise = []
				-}

simp :: Polinomio -> Polinomio 
simp [] = []
simp ((a,b):t) | b == 0 = simp t
			   | otherwise = (a,b): simp t

mult :: Monomio -> Polinomio -> Polinomio
mult _ [] = []
mult (a,b) ((c,d):t) = (a*c,b*d): mult (a,b) t


-- Falta a sortOn
{-
ordena :: Polinomio -> Polinomio
ordena [] = []
ordena ((a,b):t) = sortOn (snd) 
-}

soma :: Polinomio -> Polinomio -> Polinomio
soma ((a,b):t1) (h:t) = soma2 (a,b) (h:t) ++ soma t1 (h:t)
soma _ _ = []

soma2:: Monomio	-> Polinomio -> Polinomio	
soma2 (a,b) [] = [(a,b)]
soma2 (a,b) ((c,d):t) | b == d = [(a+c,d)]
					  | otherwise = soma2 (a,b) t 

produto :: Polinomio -> Polinomio -> Polinomio
produto [] [] = []
produto _ [] = []
produto [] _ = []
produto ((a,b):t) (h:t2) = mult2 (a,b) (h:t2) ++ produto t t2

mult2:: Monomio -> Polinomio -> Polinomio
mult2 _ [] = []
mult2 (a,b) ((c,d):t) = (a*c,b+d): mult2 (a,b) t


equiv :: Polinomio -> Polinomio -> Bool
equiv [] [] = True
equiv _  [] = False
equiv [] _  = True
equiv ((a,b):t) ((c,d):t2) = testa (a,b) ((c,d):t2) && equiv t ((c,d):t2)

testa:: Monomio -> Polinomio -> Bool	
testa _ [] = False
testa (a,b) ((c,d):t) | b == d = True
					  | otherwise = testa (a,b) t


--3 

type Mat a = [[a]]
-- [[1,2,3], [0,4,5], [0,0,6]] // each list is a row

dimOK :: Mat a -> Bool
dimOK [] = False
dimOK (h:t) = teste (length h) t

teste :: Int -> Mat a -> Bool
teste _ [] = True
teste a (h:t) | length h == a = teste a t
			  | otherwise = False


dimMat :: Mat a -> (Int,Int)
dimMat [] = (0,0)
dimMat (h:t) | dimOK (h:t) == True = tamanhoM (0,0) (h:t)
			 | otherwise = (0,0)

tamanhoM:: (Int,Int) -> Mat a -> (Int,Int)
tamanhoM (a,b) []  = (a,b)
tamanhoM (a,b) (h:t) = tamanhoM (a+1,length h) t

addMat :: Num a => Mat a -> Mat a -> Mat a
addMat = zipWith (zipWith (+))

multMat :: Num a => Mat a -> Mat a -> Mat a
multMat (x:xs) (y:ys) | snd dimMat (x:xs) == fst dimMat (y:ys) =(zipWith (*) x y) : (multMat xs (y:ys))

m2Mat:: Num a => Mat a -> Mat a -> Mat a
m2Mat (x:xs) (y:ys) |

zipWMat :: (a -> b -> c) -> Mat a -> Mat b -> Mat c
zipWMat = zipWith . zipWith
 -- f(g(x)) 
 -- g(x) = 2º zipWith
 -- f(x) = 1º zipWith
 -- f(zipWith) = zipWith(zipWith) 

-- Não sei
{-  triSup :: Num a => Mat a -> Bool
	triSup [] = False
	triSup (x:xs) = 
-}

-- Não sei
rotateLeft :: Mat a -> Mat a 
rotateLeft (h:t) = 