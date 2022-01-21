data BTree a = Empty
			 | Node a (BTree a) (BTree a)
		   deriving Show

altura :: BTree a -> Int
altura Empty = 0
altura (Node r e d) = max (1 + altura e) (1 + altura d)

folhas :: BTree a -> Int
folhas Empty = 0
folhas (Node r Empty Empty) = 1
folhas (Node r e d) = folhas e + folhas d

prune :: Int -> BTree a -> BTree a
prune _ Empty = Empty 
prune 0 _ = Empty
prune x (Node r e d) = Node r (prune (x-1) e) (prune (x-1) d)

path :: [Bool] -> BTree a -> [a]
path _ Empty = []
path [] (Node r e d) = [r]
path (x:xs) (Node r e d) | x == True = r:path xs d
						 | x == False = r:path xs e
						 | otherwise = []

zipWithBT :: (a -> b -> c) -> BTree a -> BTree b -> BTree c
zipWithBT f (Node r1 e1 d1) (Node r2 e2 d2) = Node (f r1 r2) (zipWithBT f e1 e2) (zipWithBT f d1 d2)
zipWithBT _ _ _ = Empty

-- unzip


--2 

minimo :: Ord a => BTree a -> a
minimo (Node r Empty _) = r
minimo (Node r e d) = minimo e
-- O Valor minimo de uma BTree é sempre no lado esquerdo
-- O Valor Máximo de uma BTree é sempre do lado direito 

semMinimo :: Ord a => BTree a -> BTree a
semMinimo Empty = Empty
semMinimo (Node r Empty _) = Empty
semMinimo (Node r e d) = Node r (semMinimo e) d

minSmin :: Ord a => BTree a -> (a,BTree a) 
minSmin Empty = (minimo Empty, semMinimo Empty)
minSmin (Node r e d) = (minimo e, Node r (semMinimo e) d)

remove :: Ord a => a -> BTree a -> BTree a
remove _ Empty = Empty
remove x (Node r e d) | x < r = Node r (remove x e) d
					  | x > r = Node r e (remove x d)
					  | otherwise = Node (fst (minSmin d)) e (snd (minSmin d))


type Aluno = (Numero,Nome,Regime,Classificacao)
type Numero = Int
type Nome = String
data Regime = ORD | TE | MEL deriving Show
data Classificacao = Aprov Int
				   | Rep
				   | Faltou
	deriving Show
type Turma = BTree Aluno


inscNum :: Numero -> Turma -> Bool
inscNum _ Empty = False
inscNum x (Node (num,_,_,_) e d) | x == num = True
					   			 | otherwise = (inscNum x e) || (inscNum x d) 

inscNome :: Nome -> Turma -> Bool
inscNome _ Empty = False
inscNome x (Node (_,nome,_,_) e d) | x == nome = True
								   | otherwise = (inscNome x e) || (inscNome x d)

trabEst :: Turma -> [(Numero,Nome)]
trabEst Empty = []
trabEst (Node (num,nome,reg,_) e d) | reg == TE = [(num,nome)] ++  trabEst e ++ trabEst d
									| otherwise = trabEst e ++ trabEst d

nota :: Numero -> Turma -> Maybe Classificacao
nota _ Empty = Nothing
nota x (Node (num,nome,_,clas) e d)   | x == num = Just clas
								   	  | x > num = nota x d
								      | x < num = nota x e
								      | otherwise = Nothing

percFaltas :: Turma -> Float
percFaltas Empty = 0
percFaltas x = (qmFalta x) / (totalAl x) * 100

totalAl :: Turma -> Float
totalAl Empty = 0
totalAl (Node r e d) = 1 + totalAl e + totalAl d 

qmFalta:: Turma -> Float 
qmFalta Empty = 0 
qmFalta (Node (_,_,_,clas) e d) | clas == Faltou = 1 + qmFalta e + qmFalta d
								| otherwise = qmFalta e + qmFalta d


mediaAprov :: Turma -> Float
mediaAprov Empty = 0
mediaAprov x = (media2 x / totalAl x) * 100


media2 :: Turma -> Float
media2 Empty = 0
media2 (Node (_,_,_,Aprov nota) e d) = fromIntegral nota + media2 e + media2 d
media2 (Node r e d) = media2 e + media2 d
