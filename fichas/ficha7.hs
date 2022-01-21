


data ExpInt = Const Int
| Simetrico ExpInt
| Mais ExpInt ExpInt
| Menos ExpInt ExpInt
| Mult ExpInt ExpInt

calcula :: ExpInt -> Int
calcula (Const a) = a
calcula (Simetrico a) = (- calcula a)
calcula (Mais a b) = calcula a + calcula b
calcula (Menos a b) = calcula a - calcula b
calcula (Mult a b) = calcula a * calcula b

infixa :: ExpInt -> String
infixa (Const num) = show num
infixa (Simetrico exp) = "(-" ++ infixa exp ++ ")"
infixa (Mais a b) = '(':infixa a ++ "+" ++ infixa b ++ ")"
infixa (Menos a b) = '(':infixa a ++ "-" ++ infixa b ++ ")"
infixa (Mult a b) = '(':infixa a ++ "*" ++ infixa b ++ ")"

posfixa :: ExpInt -> String
posfixa (Const num) = show num ++ " "
posfixa (Simetrico exp) = '-':posfixa exp
posfixa (Mais a b) = posfixa a ++ posfixa b ++ "+ "
posfixa (Menos a b) = posfixa a ++ posfixa b ++ "- "
posfixa (Mult a b) = posfixa a ++ posfixa b ++ "* "


data RTree a = R a [RTree a]

soma :: Num a => RTree a -> a
soma (R a []) = a
soma (R x xs) = x + sum (map (soma xs))

altura :: RTree a -> Int
altura (R a []) = 1
altura (R x xs) = 1 + maximum(map (altura) xs)
 -- Usamos maximum porque não estamos a procura do maior entre 2 argumentos mas sim do maior valor na árvore em questão de altura

prune :: Int -> RTree a -> RTree a
prune 0 (R x xs) = R x []
prune y (R x xs) = (R x (prune (y-1) xs))

-- Copiei parte da solução da sofia tava um bocado perdido, mas é compreensível o que ela fez
-- Começa para gerar a reverse para a simetria e depois aplica a função mirror a isso
mirror :: RTree a -> RTree a
mirror (R x xs) = R e (mirror (reverse xs))) 

{-
Consideremos a árvore :
		F
	B 	   G
 A 	  D   	  I
 	C   E 	H

Numa postOrder original o padrão seria : F-B-A-A-A-B-D-C-C-C-D-E-E-E-D-B-F-G-G-I-H-H-H-I-I-G-F
-}
-- 93% de certeza neste exercício 
postorder :: RTree a -> [a]
postorder (R e []) = [e]
postorder (R x xs) = concat (postorder xs) ++ [x]

data BTree a = Empty | Node a (BTree a) (BTree a)

-- Leaf Trees : Informação contida nas folhas

data LTree a = Tip a | Fork (LTree a) (LTree a)

ltSum:: Num a => LTree a -> a
ltSum Tip a = a
ltSum (Fork a b) = ltSum a + ltSum b

listaLT:: LTree a -> [a] 
listaLT Tip a = [a]
listaLT (Fork a b) = [listaLT a] ++ [listaLT b]

ltHeight :: LTree a -> Int
ltHeight Tip a = 0
ltHeight (Fork a b) = 1 + (max(ltHeight a)(ltHeight b))


-- Full Trees: Informação contida nos nodos + informação contida nas trees
data FTree a b = Leaf b | No a (FTree a b) (FTree a b)

splitFTree :: FTree a b -> (BTree a , LTree b)
splitFTree Leaf a = (Empty, Tip a)
splitFTree (No a b c) = (Node a (fst (splitFTree b)) (fst (splitFTree c)), Fork (snd(splitFTree b)) (snd(splitFTree c)))

joinTrees :: BTree a -> LTree b -> Maybe (FTree a b)
joinTrees Empty (Tip a) = Just (Leaf a) 
joinTrees (Node r e d) (Fork a b) = Just (No r (joinTrees e a) (joinTrees d b))
joinTrees _ _ = Nothing