--Assumindo a estrutura de sinônimos abaixo:
--type Nome = String
--type Idade = Int
--type Telefone = Int
--type Pessoa = (Nome, Idade , Telefone)

--Crie uma função que recebe quatro pessoas e deve retornar uma String contendo nome e telefone das pessoas cujas
-- idades são maiores ou iguais a média das idades das quatro pessoas passadas como parâmetro.

type Nome = String
type Idade = Int
type Telefone = Int
type Pessoa = (Nome, Idade, Telefone)

nome ::Pessoa -> String
nome (n, i, t) = n

idade :: Pessoa -> Float
idade (n, i, t) = idadePessoa
      where idadePessoa = fromIntegral i :: Float

telefone :: Pessoa -> String
telefone (n, i, t) = show t

dadosPessoa :: Pessoa -> Pessoa -> Pessoa -> Pessoa -> String
dadosPessoa (nome0, idade0, telef0) (nome1, idade1, telef1) (nome2, idade2, telef2) (nome3, idade3, telef3) = operacaoIdade (nome0, idade0, telef0) somaIdade ++ operacaoIdade (nome1, idade1, telef1) somaIdade ++ operacaoIdade (nome2, idade2, telef2) somaIdade ++ operacaoIdade (nome3, idade3, telef3) somaIdade
     where somaIdade = fromIntegral(idade0 + idade1 + idade2 + idade3)

mediaIdade :: Float -> Float
mediaIdade y = y / 4

operacaoIdade :: Pessoa -> Float -> String
operacaoIdade (n, i, t) x | idade(n, i, t) > mediaIdade x = nome(n, i, t) ++ " " ++ telefone (n, i, t)
                          | otherwise = " "

--dadosPessoa ("joao", 21, 122) ("gabriel", 19, 453) ("lunna", 20, 865) ("abraao", 22, 474)