{--
Crie uma função "organizaNomes" em Haskell que recebe uma lista de strings com nomes de pessoas
(pode ser só o primeiro nome) e retorna uma lista de tuplas onde o primeiro elemento da tupla é um
caractere e o segundo é uma lista com os nomes da lista inicial que começam com este caractere. Por exemplo:

-> organizaNomes ["jose", "lucas", "joao", "carlos", "antonio", "alfredo", "zoe"]

-> [('a', ["alfredo", "antonio"]), ('c', ["carlos"]), ('j', ["joao", "jose"]), ('l', ["lucas"]), ('z', ["zoe"])]
--}

organizaNomes :: [String] -> [(Char, [String])]

organizaNomes [] = []

organizaNomes (a:as) = [(head a, [b | b <- (a:as), head b == head a])] ++ organizaNomes [b | b <- (a:as), not (head b == head a)]

-- a primeira parte retorna uma lista de tuplas, em que o 1º elemento da tupla é a primeira letra do nome (Char) e o 2º elemento é uma lista de Strings de nomes (o head a é o caractere)
-- que começam com a letra destacada, usando uma condição (head b == head a); e depois chama a função novamente para não usar os nomes repetidos
-- quando a lista toda é percorrida, informa uma mensagem de que a lista, no momento, está vazia

-- usa concatenação de listas p/ adicionar os nomes nas listas
-- usar funções filter/map??

--prototipo
{--
getCaracter :: String -> (Char, [String])
getCaracter [] = error "Lista vazia"
getCaracter (a:as) = (a,[a:as])

organizaNomes :: [String] -> [(Char, [String])]
organizaNomes [] = error "Lista vazia"
organizaNomes n = map getCaracter n
--}