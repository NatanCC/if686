data Shape = Circle Float
    | Retangle Float Float
area :: Shape -> Float
area (Circle x) = (pi * (x ** 2))
area (Retangle x y) = x * y


type HorasAula = Int
type Disciplinas = [String]
data Dias = Domingo
    | Segunda HorasAula Disciplinas
    | Terca HorasAula Disciplinas
    | Quarta HorasAula Disciplinas
    | Quinta HorasAula Disciplinas
    | Sexta HorasAula Disciplinas
    | Sabado

fimDeSemana :: Dias -> Bool
fimDeSemana Domingo = True
fimDeSemana Sabado = True
fimDeSemana _ = False

plcClass :: Dias -> Bool
plcClass (Domingo) = False
plcClass (Sabado) = False
plcClass (Segunda _ l) = plcIn l "PLC"
plcClass (Terca _ l) = plcIn l "PLC"
plcClass (Quarta _ l) = plcIn l "PLC"
plcClass (Quinta _ l) = plcIn l "PLC"
plcClass (Sexta _ l) = plcIn l "PLC"

plcIn :: Disciplinas -> String -> Bool
plcIn [] _ = False
plcIn (x:xs) s
    | x == s = True
    | otherwise = plcIn xs s

data Expr = Lit Int
    | Add Expr Expr
    | Sub Expr Expr
        deriving (Show)    

data List t = Nil
    | Cons t (List t)

data Tree t = NilT
    | Node t (Tree t) (Tree t)
        deriving (Show, Eq)


showExpr :: Expr -> String
showExpr e = show e


toList :: List t -> [t]
toList Nil = []
toList (Cons x y) = (x: toList y)

lista :: List Int
lista = Cons 5 (Cons 4 (Nil))

fromList :: [t] -> List t
fromList [] = Nil
fromList (x:xs) = (Cons x (fromList xs))

depth :: Tree t -> Int
depth NilT = 0
depth (Node x NilT NilT) = 0
depth (Node x l r) = max (1 + depth l) (1 + depth r)

collapse :: Tree t -> [t]
collapse NilT = []
collapse (Node x l r) = [x]++ collapse l ++ collapse r

tree ::Tree Int
tree = (Node 43 (Node 32 (Node 5 (NilT) (Node 2 (NilT) (NilT))) (NilT)) (Node 1 (NilT) (Node 98 (NilT) (NilT))))

bfs :: (Eq t) => Tree t -> t -> Bool
bfs NilT _ = False
bfs (Node n l r) x
    | n == x = True
    | otherwise = False || bfs l x|| bfs r x

mapTree :: (t -> u) -> Tree t -> Tree u
mapTree _ NilT = NilT
mapTree f (Node n l r) = (Node (f n) (mapTree f l) (mapTree f r))