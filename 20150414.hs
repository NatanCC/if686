----------------------------------------------------------
-- Trabalho 7 aluno: Natanael Souza dos Santos login: nss
----------------------------------------------------------

-- Questão 1
compose :: (Num t) => (t -> t) -> [t -> t] -> [t -> t]
compose f l = map comp l
    where comp g = f.g

-- Questão 2

data Graph t = NilG
    | NodeG t [(t, Int)] (Graph t)
        deriving (Show, Eq)

mapGraph :: (a -> b) -> Graph a -> Graph b
mapGraph _ NilG = NilG
mapGraph f (NodeG n adjL g) = ((NodeG (f n)) (mapDupList f adjL) (mapGraph f g))

mapDupList :: (a -> b) -> [(a, Int)] -> [(b, Int)]
mapDupList _ [] = []
mapDupList f ((v, n):xs) = (((f v), n): mapDupList f xs)

grafo :: Graph Int
grafo = (NodeG 1 [(2, 2), (3, 2)] (NodeG 2 [(3, 1)] (NodeG 3 [] (NilG))))

foldGraph :: (Num a) => (a -> a -> a) -> Graph a -> a
foldGraph _ NilG = 0
foldGraph f (NodeG v l g) = f v (foldGraph f 	g)

-- Questão 3
data Tree t = NilT
    | Node t (Tree t) (Tree t)
        deriving (Show, Eq)


filterTree :: (Eq a) => (a -> Bool) -> Tree a -> [Tree a]
filterTree _ NilT = [NilT]
filterTree f (Node n tl tr)
    | f n = [filterT f (Node n tl tr)] ++ filterTree f tl ++ filterTree f tr
    | otherwise = [filterT f tl] ++ [filterT f tr]

filterT :: (a -> Bool)  -> Tree a -> Tree a
filterT _ NilT = NilT
filterT f (Node n tl tr)
    | f n = (Node n (filterT f tl) (filterT f tr))
    | otherwise = NilT
