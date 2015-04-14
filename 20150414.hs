----------------------------------------------------------
-- Trabalho 7 aluno: Natanael Souza dos Santos login: nss
----------------------------------------------------------

-- Questão 1
compose :: (Num t) => (t -> t) -> [t -> t] -> [t -> t]
compose f l = map comp l
    where comp g = f.g

-- Questão 2

data Graph t = NilG
    | Node t [(t, Int)] (Graph t)
        deriving (Show, Eq)

mapGraph :: (a -> b) -> Graph a -> Graph b
mapGraph _ NilG = NilG
mapGraph f (Node n adjL g) = ((Node (f n)) (mapDupList f adjL) (mapGraph f g))

mapDupList :: (a -> b) -> [(a, Int)] -> [(b, Int)]
mapDupList _ [] = []
mapDupList f ((v, n):xs) = (((f v), n): mapDupList f xs)

grafo :: Graph Int
grafo = (Node 1 [(2, 2), (3, 2)] (Node 2 [(3, 1)] (Node 3 [] (NilG))))

foldGraph :: (Num a) => (a -> a -> a) -> Graph a -> a
foldGraph _ NilG = 0
foldGraph f (Node v l g) = f v (foldGraph f 	g)

-- Questão 3

data Tree t = NilT
    | Node t (Tree t) (Tree t)
        deriving (Show, Eq)

filterTree :: (a -> Bool) -> Tree a -> [Tree a]
filterTree _ NilT = [NilT]

