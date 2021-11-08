pMaior:: Ord a => [a] -> Int
pMaior [_] = 0 -- Retorna 0 se for só 1 elemento
pMaior (x:xs) = pMA (x, 0 , 1) xs

		
pMA:: Ord a => (a,Int,Int) -> [a] -> Int
pMA (m,pm,pa) [] = pm
pMA (m,pm,pa) (h:t) | h > m = pMA (h,pa,pa+1) t
					| otherwise = pMA (m,pm,pa+1) t


{-
pm = posição maior
pa = posição atual
m = valor maior
[0,1,2,3,4]
 														
 pMA (0,0,1) [1,2,3,4]
 pMA (0,0,1) (1:[2,3,4]) | 1 > 0 = pMA (1,1,2) [2,3,4]       pm +1 , pa +1
 pMA (1,1,2) (2:[3,4]) | 2 > 1 = pMA (2,2,3) [3,4] 			 pm +1 , pa+1
 pMA (2,1,2) (3:[4]) | 3 > 2 = pMA (3,3,4) [4]
 pMA (3,3,4) (4:[]) | 4 > 3 = pMA (4,4,5) []

 pMA (4,4,5) [] = 4

  [0,1,2,3,4]
 



-}