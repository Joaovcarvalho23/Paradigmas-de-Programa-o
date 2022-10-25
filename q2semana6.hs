-- Segundo Helder Nakaya, membro do comitê científico da SBI (Sociedade Brasileira de Infectologia), 
-- para medir a eficácia de uma vacina "o que se faz é comparar o número de infectados no grupo de 
-- controle - aquele que recebe placebo ou outra vacina que não é a que está sendo testada - , com 
-- o número de infectados no grupo que recebeu a vacina em desenvolvimento". Por exemplo, considerando 
-- 20% dos voluntários que receberam a vacina tiveram a doença, e que 70% dos que receberam o placebo 
-- adoeceram, a conta a ser feita é a seguinte: um menos o valor resultante da divisão de 20% por 70%. 
-- Neste caso hipotético, a eficácia é de 71,4%. Assim, crie um programa em Haskell que lê do usuário 
-- o nome da vacina, a porcentagem de pessoas que se vacinaram e tiveram a doença, e a porcentagem dos 
-- que tomaram placebo e tiveram a doença. O processo se repete até que se digite a string vazia para o 
-- nome da vacina. Por último, seu programa deve salvar um arquivo com o nome "vacinas.txt" cujo conteúdo 
-- por linha deve ser [nomeVacina];[%infectados vacina];[% infectados placebo];[efetividade da vacina]. 
-- Segue um exemplo de conteúdo abaixo:

-- Pfizer;0.1;0.8;0.87

-- Astrazeneca;0.045;0.65;0.93

-- Moderna;0.03;0.087;0.65

-- SinoVac;0.2;0.7;0.71

-- Novavax;0.125;0.73;0.83

import System.Exit (exitSuccess)


main :: IO()
main = do putStr "Digite o nome da vacina: "
          nomeVac <- getLine
          if nomeVac == "" then exitSuccess else putStr "" 
          putStr "Informe a porcentagem de pessoas que se vacinaram e tiveram a doença: "
          vacinadosInfect <- readLn :: IO Float
          putStr "Digite a porcentagem dos que tomaram placebo e tiveram a doença: "
          placeboInfect <- readLn
          putStrLn "\nINFORMAÇÕES GUARDADAS NO ARQUIVO!\nCaso deseje encerrar o programa, pressione 'Enter'\n"
          appendFile "C:/dev/vacina.txt" (nomeVac ++ "; " ++ show(vacinadosInfect) ++ "; " ++ show(placeboInfect) ++ "; " ++ (show(1 - vacinadosInfect/placeboInfect)) ++ "\n")
          main