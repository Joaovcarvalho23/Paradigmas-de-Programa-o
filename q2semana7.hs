-- Utilizando a linguagem funcional Haskell, defina uma função bag que recebe uma lista 
-- de elementos e retorna uma lista de pares, onde o primeiro elemento de cada par é um elemento
-- da lista original e o segundo é o número de ocorrências deste elemento. Nesta segunda lista,
-- cada elemento só ocorre uma vez. Por exemplo, bag [a,b,a,c,a,b] = [(a,3),(b,2),(c,1)]. 

-- primeiro sort, depois group
-- o sort ordena a lista do menor para o maior
-- o group transforma a lista em sublistas de um mesmo grupo/valor
-- pega o tamanho do group, algo do tipo "length (group (sort xs))"

import Data.List

bag :: Eq j => [j] -> [(j, Int)]
bag [] = []
bag (a:as) = (a, length (filter (a ==) as) + 1) : bag (filter (a /=) as)

--exemplo ghci:
--bag ['a','b','a','c','a','b']