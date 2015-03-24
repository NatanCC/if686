mergeSort :: [Int] -> [Int]
mergeSort [x] = [x]
mergeSort a = merge (mergeSort (divideLeft a 0 ((count a)`div`2))) (mergeSort (divideRight a 0 ((count a) `div`2)))



count :: [Int] -> Int
count [] = 0
count (x:xs) = 1 + count xs

divideLeft :: [Int] -> Int -> Int -> [Int]
divideLeft (x:xs) a b | a < b = (x:(divideLeft xs (a+1) b))
                      | otherwise = []

divideRight :: [Int] -> Int -> Int -> [Int]
divideRight [] a b = []
divideRight (x:xs) a b | a > 2*b = []
                        | a >= b = (x:(divideRight xs (a+1) b))
                        |otherwise = divideRight xs (a+1) b

merge :: [Int] -> [Int] -> [Int]
merge [] [] = []
merge (a:as) (b:bs) | a <= b = (a: merge as (b:bs))
                    | otherwise = (b: merge (a:as) bs)
merge (a:as) [] = (a: merge as [])
merge [] (b:bs) = (b: merge [] bs)

--Complexidade
-- divideLeft = O(n)
-- divideRight = O(n)
-- count = O(n)
-- merge = O(n)
-- mergeSort = O(n^2)

--------------------------------------------------------------------------------
-- exercicios da aula
--------------------------------------------------------------------------------

menorMaior :: Int -> Int -> Int -> (Int, Int)
menorMaior a b c | a <= b && a <= c &&  b >= c = (a, b)
				| b <= a && b <= c &&  a >= c = (b, a)
				| c <= a && c <= b &&  a >= c = (c, a)
				| a <= b && a <= c &&  c >= b = (a, c)
				| b <= a && b <= c &&  c >= a = (b, c)
				| otherwise = (c, b)

ordenaTripla :: (Int, Int, Int) -> (Int, Int, Int)
ordenaTripla (a, b, c) | a >= b && b >= c = (c, b, a)
						| a >= c && c >= b = (b, c, a)
						| b >= a && a >= c = (c, a, b)
						| b >= c &&  c >= a = (a, c, b)
						| c >= a && a >= b = (b, a, c)
						| otherwise = (a, b, c)


type Ponto = (Float, Float)
type Reta = (Ponto, Ponto)


firstCoord :: Ponto -> Float
firstCoord (a, b) = a

secondCoord :: Ponto -> Float
secondCoord (a, b) = b

isVertical :: Reta -> Bool
isVertical ((x1, y1), (x2, y2)) | x1 == x2 = True
								| otherwise = False

pontoY :: Float -> Reta -> Float
pontoY a ((x1, y1), (x2, y2)) = ((y2*a)-(y2*x1)-(y1*a)+(y1*x2))/(x2-x1)

type Pessoa = String
type Livro = String
type BancoDados = [(Pessoa, Livro)]

baseExemplo :: BancoDados
baseExemplo = [("Sergio", "O Senhor dos Aneis"),
                ("Andre", "Duna"),
                ("Fernando", "Jonathan Strange & Mr. Norrell"),
                ("Fernando", "A Game of Thrones")]
-- livros emprestados

-- consultas
livros :: BancoDados -> Pessoa ->[Livro]
livros [] _ = []
livros l p = [livro | (pessoa, livro) <- l, pessoa == p ]

emprestimos:: BancoDados -> Livro -> [Pessoa]
emprestimos [] _ = []
emprestimos l li = [pessoa | (pessoa, livro) <- l, livro == li]

emprestado :: BancoDados -> Livro -> Bool
emprestado [] _ = False
emprestado ((p,l):s) li
   |l == li = True
   |otherwise = emprestado s li

qtdEmprestimos :: BancoDados -> Pessoa -> Int
qtdEmprestimos [] _ = 0
qtdEmprestimos ((p, l):s) pe | pe == p = 1 + qtdEmprestimos s pe
                             | otherwise = qtdEmprestimos s pe

--atualizações

emprestar :: BancoDados -> Pessoa -> Livro -> BancoDados
emprestar [] p l = ((p, l):[])
emprestar bd pe li | emprestado bd li = bd
                           | otherwise = bd++[(pe, li)]

devolver :: BancoDados -> Pessoa -> Livro -> BancoDados
devolver [] _ _ = []
devolver s pessoa livro = [(p, l) | (p, l) <- s, (p, l) /= (pessoa, livro)]

--quickSort
quickSort :: [Int] -> [Int]
quickSort [] = []
quickSort (x:xs) = quickSort [a | a <- xs, a <= x ]++[x]++ quickSort [a | a <- xs, a > x]

--Processamento de Texto

getWord :: String -> String
getWord [] = []
getWord (x:xs)
  | [x] /= " " = (x:(getWord xs))
  | otherwise = ""

dropWord :: String -> String
dropWord [] = []
dropWord (x:xs)
  | [x] == " " = (x:xs)
  | otherwise = dropWord xs

dropSpace :: String -> String
dropSpace [] = []
dropSpace (x:xs)
  | [x] == " " = dropSpace xs
  | otherwise = (x:xs)

type Word = String

--splitWords :: String -> [Word]
