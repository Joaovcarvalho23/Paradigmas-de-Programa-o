-- Queremos descobrir quem são os servidores que recebem acima da média dos salários de outros
-- servidores. Assim, crie uma função em Haskell que recebe uma lista de pares onde o primeiro elemento
-- do par é uma string com o CPF do servidor e a segunda é um número real com o seu salário. Tal função
-- deve retornar uma lista com os CPFs de servidores que recebem acima da média salarial de todos os servidores.

type Cpf = String
type Salario = Float
type Servidores = [Servidor]
type Servidor = (Cpf, Salario)

cpf :: Servidores -> [String]
cpf l = [fst x | x <- l]

salario :: Servidores -> [Float]
salario l = [snd x | x <- l]

mediaSalario :: [Float] -> Float
mediaSalario [] = 0.0
mediaSalario l = (sum(l)) / fromIntegral(length(l))

comparaSalario :: Float -> Servidor -> Bool
comparaSalario x l = (snd l) > x

cpfAcimaDaMedia :: Servidores -> [String]
cpfAcimaDaMedia [] = ["CPF não encontrado!"]
cpfAcimaDaMedia l = cpf (filter x l)
                    where x = (comparaSalario(mediaSalario (salario l)))


-- Exemplo para ghci
--cpfAcimaDaMedia [("213.345.632-32", 23.4), ("123.456.789-10", 2765.2), ("432.743.862-32", 2023.7), ("124.865.234-98", 843.3)]