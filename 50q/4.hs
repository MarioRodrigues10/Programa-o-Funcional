corresponde :: [a] -> Int -> a
corresponde (x:xs) y = if y == 0 then x else corresponde xs (y-1)



{- Aqui o que nos pede é achar um elemento que está em uma posição específica em
uma lista. 
	 
	 	Considerando, (x:xs) a lista e  Y a posição que quero achar o meu elemento, vejamos o seguinte exemplo.

	 	Exemplo: [1,2,3,4] 2 
	 		Y = 2 ; (x:xs) = (1:[2,3,4])

	 			O que é que o programa vai fazer? 

	 			  Y == 0 ? Não , então vamos voltar a executar o programa e tirar 1 ao Y

	 			  Y = 1 ; (x:xs) = (2:[3,4])

	 			  Y == 0 ? Não , então vamos voltar a executar o programa e tirar 1 ao Y

	 			  Y = 0 ; (x:xs) = (3:[4])

	 			  Y == 0 ? Sim, então output = 3

	 		Output: 3

	 		  	General questions: 

	 		  	Porque é que o output é 3 e não 2?

	 		  	 - Em uma lista o primeiro elemento é o 0 e por aí em diante, então.
	 		  	 Em uma lista [1,2,3,4] , o 3 vai estar na 2ª posição




	
	


}