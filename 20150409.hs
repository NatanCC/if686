----------------------------------------------------------
-- Trabalho 6 aluno: Natanael Souza dos Santos login: nss
----------------------------------------------------------
{-1. Defina um tipo de dados alg ́ebrico que represente um grafo direcionado
com pesos nas arestas e que guarda elementos de um tipo arbitr ́ario em seus
n ́os. Torne poss ́ıvel que grafos desse tipo sejam compar ́aveis (igualdade) e
transform ́aveis em Strings.-}

data List t = Nil
    | Cons (t, Int) (List t)
        deriving (Show, Eq)

data Graph t = NilG
    | Node t (List t) (Graph t)
        deriving (Show, Eq)

{-2. Implemente uma fuņc̃ao que, dado um valor, faz uma busca em profundi-
dade em um grafo e devolve True ou False, caso esse valor esteja ou ñao em
um dos ńos do grafo-}