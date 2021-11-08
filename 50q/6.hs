take':: Int -> [a] -> [a]
take' n [] = [] 
take' n (x:xs) = if n == 0 then []
						   else x:take' (n-1) xs


{- Take : Aqui é nos dado um valor n de elementos que vamos ter que "pegar" da lista
					
	Caso a lista seja vazia, o output vai ser automaticamente uma lista vazia
	Caso não seja, temos o n que vai "pegar" elementos enquanto for diferente de 0. 
	Quando o n for igual a 0, signfica que já não há mais elementos para retirar, e teremos o nosso output.

	Exemplo: take' 2 [1,2,3,4]

			 take' 2 [] ? Não, temos uma lista [1,2,3,4]

			 take' 2 (1:[2,3,4])

			 n == 0 ? Não 

			 1:take' (2-1) [2,3,4]

			 take' 1 [2,3,4]
			 
			 take' 2 [] ? Não, temos uma lista [2,3,4]


	

-}