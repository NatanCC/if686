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

lookAndSay :: Int -> String
lookAndSay 0 = "0"
lookAndSay 1 = "1"
lookAndSay x = intListToDigit sequencia
            where
			listaSequencia = lookAndSay3 [1] (x-1)
			sequencia = head(reverseList listaSequencia)
			


lookAndSay2 :: [Int] -> [Int]
lookAndSay2 [0] = []
lookAndSay2 (x:xs) = say((push x (x:xs))) ++ (lookAndSay2 (pop x (x:xs)))
lookAndSay2 [] = []

lookAndSay3 :: [Int] -> Int -> [[Int]]
lookAndSay3 _ 0 = []
lookAndSay3 x y = [w] ++ lookAndSay3 w (y-1)
                where
				w = lookAndSay2 x

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

-- Funções para transformar a list de sequencia em um Int

count2 :: [Int] -> Int -> [Int]
count2 [] _ = []
count2 (x:xs) y = (y: (count2 xs (y*10)))

reverseList [] = []
reverseList (x:xs) = reverseList xs ++ [x]

listToInt :: [Int] -> [Int] -> Int
listToInt [] _ = 0
listToInt _ [] = 0
listToInt (x:xs) (y:ys) = (x*y) + listToInt xs ys

intListToDigit :: [Int] -> String
intListToDigit [] = []
intListToDigit (x:xs) = show x ++ intListToDigit xs

----------------------------------------------------------
-- Exercício da aula ps: aula com monitor
----------------------------------------------------------

afd :: String ->  [Int] -> [(Int, Int, String)] -> Int -> [Int] -> Bool
afd cad (est:ys) transi ini fin = percorre transi ini cad fin


estado :: [(Int, Int, String)] -> Int -> String -> Int
estado ((ori, dest, entr):ws) est cad
                                    | ori == est && entr == cad = dest
									| otherwise = estado ws est cad
estado [] _ _ = 0


percorre :: [(Int, Int, String)] -> Int -> String -> [Int] -> Bool
percorre _ est [] fin = aceita est fin
percorre transi est (x:xs) fin = percorre transi (estado transi est [x]) xs fin


aceita :: Int -> [Int] -> Bool
aceita _ [] =  False
aceita a (x:xs)
                | a == x = True
				| otherwise = aceita a xs

