----------------------------------------------------------
-- Trabalho 6 aluno: Natanael Souza dos Santos login: nss
----------------------------------------------------------
{-1. Defina um tipo de dados alg ́ebrico que represente um grafo direcionado
com pesos nas arestas e que guarda elementos de um tipo arbitr ́ario em seus
n ́os. Torne poss ́ıvel que grafos desse tipo sejam compar ́aveis (igualdade) e
transform ́aveis em Strings.-}
import Data.Char


data Graph t = NilG
    | Node t [(t, Int)] (Graph t)
        deriving (Show, Eq)

{-2. Implemente uma fuņc̃ao que, dado um valor, faz uma busca em profundi-
dade em um grafo e devolve True ou False, caso esse valor esteja ou ñao em
um dos ńos do grafo-}

grafo :: Graph Int
grafo = (Node 1 [(2, 2), (3, 2)] (Node 2 [(3, 1)] (Node 3 [] (NilG))))

dfs :: (Eq t) => Graph t -> t -> Bool
dfs NilG _ = False
dfs (Node n l g) v = dentro nodes v
    where
    	grafo = (Node n l g)
    	node = fst (head l)
    	nodes = dfs1 grafo n [n]


dfs1 :: (Eq t) => Graph t -> t -> [t] -> [t]
dfs1 NilG _ l
    | tail l == [] = []
    | otherwise = l
dfs1 (Node n l g) node discoveredL
    | n == node = (percorre l discoveredL)++discoveredL ++ dfs1 (Node n l g) (head(percorre l discoveredL)) (percorre1 l discoveredL)
    | otherwise = dfs1 g node discoveredL

dentro :: (Eq t) => [t] -> t -> Bool
dentro [] _ = False
dentro (x:xs) a
    | a == x = True
    | otherwise = dentro xs a

percorre :: (Eq t) => [(t, Int)] -> [t] -> [t]
percorre [] l = []
percorre ((v, x):s) l
    | dentro l v = percorre s l
    | otherwise = [v]
	
	
percorre1 :: (Eq t) => [(t, Int)] -> [t] -> [t]
percorre1 [] l = l
percorre1 ((v, x):s) l
    | dentro l v = percorre1 s l
    | otherwise = (v:l)
	
----------------------------------------------------------
-- Exercício da aula
----------------------------------------------------------


map1 :: (v -> u) -> [v] -> [u]
map1 f [] = []
map1 f (x:xs) = ((f x):(map1 f xs))

calculaRaiz l = map2 sqrt l

position  :: Char -> Int
position a = (ord a) - 96

posicaoAlfabeto :: String -> [Int]
posicaoAlfabeto s = map2 position s

map2 :: (v -> u) -> [v] -> [u]
map2 f [] = []
map2 f l = [f a | a <- l]

member :: (Eq t) => t -> [t] -> Bool
member e l = foldr (||) False (map (== e) l)
------------------- questão de lucifer inicio

-------------------- questão de lucifer fim

sToInt :: String -> [Int]
sToInt x = map1 position x

sumList :: [Int] -> Int
sumList xs = foldr (+) 0 xs

sListToIList :: [String] -> [Int]
sListToIList (x:xs) = (sumList (sToInt x): sListToIList xs)
sListToIList [] = []

--recebe duas listas de inteiros, e retorna uma lista contendo
--a soma dos elementos de cada lista
sLI :: [Int] -> [Int] -> [Int]
sLI x y = (sumList x):[sumList y]

sLI1 :: [String] -> [Int]
sLI1 x = foldr (sLI) [] (map sToInt x)
	
removeMenor0 :: [Int] -> [Int]
removeMenor0 x = filter maior0 x

maior0 :: Int -> Bool
maior0 x = x > 0

inter :: (Ord t, Eq t) => [t] -> [t] -> [t]
inter l1 l2 = removeDup (filter f l1)
    where f x = x `elem` l2

diff :: (Ord t, Eq t) => [t] -> [t] -> [t]
diff l1 l2 = removeDup (filter f l1)
    where f x = not (x `elem` l2)

removeDup :: (Eq t) => [t] -> [t]
removeDup [] = []
removeDup (x:xs)
    | x `elem` xs = removeDup xs
    | otherwise = x:removeDup xs