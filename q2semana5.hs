-- Blockchain é basicamente um banco de dados distribuído onde os dados são armazenados em blocos onde cada bloco 
-- possui transações financeiras e uma referência para o próximo bloco formando uma corrente. Assumindo os tipos 
-- de dados abaixo, crie uma função em Haskell que dado um Bloco e uma String de uma conta retornam uma tupla-2, 
-- no qual o primeiro elemento corresponde a média dos valores das transações em toda a corrente nas quais a conta 
-- passada como parâmetro era a origem (de), e o segundo elemento contém também a média dos valores mas quando a conta 
-- era destino (para).

-- data Transacao = Transacao { de :: String -- Conta que paga

                                                -- , para :: String -- Conta que recebe

                                                 -- , valor :: Float -- Quanto está pagando

                                                -- } deriving (Show)

-- data Bloco = Bloco { indice :: Int -- Indice do bloco

                                 -- , trs :: [Transacao] -- Lista de transações de um bloco

                                 -- , proximo :: Maybe Bloco -- Proximo bloco

                                 -- } deriving (Show)

-- OBS: para definir um valor do tipo bloco é só especificar (Bloco ind trs prox), onde ind é um inteiro, trs é uma lista 
-- de transações e prox é um Maybe Bloco. O mesmo se aplica ao tipo Transacao.
--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

data Transacao = Transacao {de :: String, para :: String, valor :: Float} deriving (Show)
data Bloco = Bloco {indice :: Int, trs :: [Transacao], proximo :: Maybe Bloco} deriving (Show)

juntarListas :: Maybe Bloco -> [Transacao]
juntarListas Nothing = []
juntarListas (Just (Bloco _ trans bloco)) = trans ++ juntarListas bloco

contaMesmaOrigem :: String -> Transacao -> Bool
contaMesmaOrigem conta (Transacao origem _ _) = conta == origem

contaMesmoDestino :: String -> Transacao -> Bool
contaMesmoDestino conta (Transacao _ dest _) = conta == dest

getValorTransacao :: Transacao -> Float
getValorTransacao (Transacao _ _ v) = v

somarLista :: [Transacao] -> Float
somarLista = foldr ((+) . getValorTransacao) 0

getMediaValorTrans :: Maybe Bloco -> String -> (Float, Float)
getMediaValorTrans bloco conta = (somarLista (filter (contaMesmaOrigem conta) (juntarListas bloco)) / fromIntegral (length (filter (contaMesmaOrigem conta) (juntarListas bloco))), somarLista (filter (contaMesmoDestino conta) (juntarListas bloco)) / fromIntegral (length (filter(contaMesmoDestino conta) (juntarListas bloco))))


-- getMediaValorTrans (Just (Bloco 0 [(Transacao "conta1" "conta3" 200), (Transacao "conta1" "conta4" 80), (Transacao "conta3" "conta4" 62), (Transacao "conta1" "conta2" 124)] (Just (Bloco 1 [(Transacao "conta4" "conta2" 148), (Transacao "conta1" "conta4" 46), (Transacao "conta1" "conta2" 31), (Transacao "conta4" "conta3" 23)] (Just (Bloco 3 [(Transacao "conta2" "conta6" 148), (Transacao "conta6" "conta4" 46), (Transacao "conta1" "conta4" 31)] Nothing)))))) "conta4"