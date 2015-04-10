----------------------------------------------------------
-- Trabalho 6 aluno: Natanael Souza dos Santos login: nss
----------------------------------------------------------
{-1. Defina um tipo de dados alg ́ebrico que represente um grafo direcionado
com pesos nas arestas e que guarda elementos de um tipo arbitr ́ario em seus
n ́os. Torne poss ́ıvel que grafos desse tipo sejam compar ́aveis (igualdade) e
transform ́aveis em Strings.-}



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
    	nodes = dfs1 grafo node [node]


dfs1 :: (Eq t) => Graph t -> t -> [t] -> [t]
dfs1 NilG _ l
    | tail l == [] = []
    | otherwise = l
dfs1 (Node n l g) node discoveredL
    | n == node = dfs1 (Node n l g) (head(percorre l discoveredL)) (percorre l discoveredL)
    | otherwise = dfs1 g node discoveredL

dentro :: (Eq t) => [t] -> t -> Bool
dentro [] _ = False
dentro (x:xs) a
    | a == x = True
    | otherwise = dentro xs a

percorre :: (Eq t) => [(t, Int)] -> [t] -> [t]
percorre [] l = l
percorre ((v, x):s) l
    | dentro l v = percorre s l
    | otherwise = (v:l)


