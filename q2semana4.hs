{--
Utilizando a linguagem funcional Haskell, defina uma função que recebe uma lista de inteiros e
deve retornar uma lista de tuplas-2 onde o primeiro elemento é igual ao número da lista e o segundo]
é a soma dos dígitos deste número. No entanto, a lista resultante deve conter apenas tuplas nas quais
o primeiro elemento é um múltiplo do segundo. 
Exemplos: digitosDeMultiplos [5,12,71,8,25,3,150] -> [(5,5),(12,3),(8,8),(3,3),(150,6)]
digitosDeMultiplos [98,24,81,7773,21,1000] -> [(24,6),(81,9),(21,3),(1000,1)]
--}

somaDigitos :: Int -> Int
somaDigitos 0 = 0
somaDigitos x = (mod x 10) + somaDigitos(div x 10)

multiplo :: (Int, Int) -> Bool
multiplo (n1, n2) | mod n1 n2 == 0 = True
                  | otherwise = False

digitosDeMultiplos :: [Int] -> [(Int, Int)]
digitosDeMultiplos [] = []
digitosDeMultiplos l = filter multiplo [(n, (mod n 10) + somaDigitos (div n 10)) | n <- l]