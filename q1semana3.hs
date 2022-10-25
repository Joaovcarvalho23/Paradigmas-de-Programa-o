--Dada as funções "vendas" e "totalVendas" que utilizamos nas aulas desta semana (videoaula e slides), crie uma função "relatorio::Int -> (Int, Int, Int, Float)" que retorna um relatório dos dados até uma determinada semana passada como parâmetro. O relatório é uma tupla onde o primeiro elemento é o total de vendas até aquela semana, o segundo é o número da semana com mais vendas até aquela semana, o terceiro é a maior quantidade de vendas até aquela semana, e o quarto é a média de vendas até aquela semana. Dica: use funções intermediárias para calculcar o que se deseja. 

vendas :: Int -> Int
vendas 0 = 10
vendas 1 = 1
vendas 2 = 25
vendas 3 = 12
vendas 4 = 23

totalVendas :: Int -> Int
totalVendas n | n == 0 = vendas 0
              | otherwise = totalVendas(n-1) + vendas n


maiorQue :: Int -> Int -> Int
maiorQue a b | a>b = a
         | a == b = a
         | otherwise = b

maximoVendas :: Int -> Int
maximoVendas 0 = vendas 0
maximoVendas n = maiorQue (maximoVendas(n-1)) (vendas n)


semanaComMaisVendas :: Int -> Int
semanaComMaisVendas n | vendas n == maximoVendas n = n
                      | otherwise = semanaComMaisVendas (n-1)
                  


media :: Int -> Float
media n = (int(totalVendas n) / int(n))
   where int = fromIntegral


relatorio :: Int -> (Int, Int, Int, Float)
relatorio n = (totalVendas n, semanaComMaisVendas n, maximoVendas n, media n)