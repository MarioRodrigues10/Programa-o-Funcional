reverse':: [a] -> [a]
reverse' [] = []
reverse' (x:xs) = reverse' xs ++ [x]



{- Reverse, este exercício pede para alterarmos a ordem da lista, ou seja, o 1º passar a
ser o último, o 2º o penúlitmo e por aí em diante.
  
  Explicação: Começei por dizer que caso a lista seja vazia, o output vai ser uma lista vazia. Porque é 
 impossível inverter.
 			  No passo de baixo, chamei de novo a função reverse' para a tail e juntei usando a função
 			 predefinida ++ com a head da lista. Isto com um exemplo talvez fique mais fácil de explicar.

 			 Exemplo: 
 			   reverse' [1,2,3,4]
 			   reverse' (1:[2,3,4]) = reverse' [2,3,4] ++ [1]
 			   reverse' [2,3,4]
 			   reverse' (2:[3,4]) = reverse' [3,4] ++ [2,1]
 			   reverse' [3,4]
 			   reverse' (3:[4]) = reverse' [4] ++ [3,2,1]
 			   reverse' [4]
 			   reverse' (4:[]) = reverse' [] ++ [4,3,2,1]
 			   reverse' [] = []

 			   Output : [4,3,2,1]

			
 	

} 