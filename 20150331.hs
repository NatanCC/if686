----------------------------------------------------------
-- Trabalho 4 aluno: Natanael Souza dos Santos login: nss
----------------------------------------------------------
-- Questão 1
{-
  Vantagens: Em Haskell o poliformismo de sobrecarga é definido fora da declaração da função.
  Ou seja, não precisamos escrever várias assinaturas como Java.

  Desvantagens: Em Java podemos criar poliformismo de sobrecarga fazendo com que função tenha
  diferentes tipos de retorno.
-}

-- Questão 2


lookAndSay :: [Int] -> [Int]
lookAndSay [0] = []
lookAndSay (x:xs) = say((push x (x:xs))) ++ (lookAndSay (pop x (x:xs)))
lookAndSay [] = []

push :: Int -> [Int] -> [Int]
push _ [] = []
push x (y:ys)
    | x == y = (y:(push x ys))
	| otherwise = []
	
pop :: Int -> [Int] -> [Int]
pop _ [] = []
pop x (y:ys)
    | x == y = (pop x ys)
	| otherwise = (y:ys)
	
count :: [Int] -> Int
count [] = 0
count (x:xs) = 1 + count xs

say :: [Int] -> [Int]
say [] = []
say (x:xs) = [(count (x:xs)), x]
