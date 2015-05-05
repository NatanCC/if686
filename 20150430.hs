

data Failable t = Valor t
    | Erro String

instance Monad Failable where
	(>>=) (Valor t) f = f t
	(>>=) (Erro t) f = Erro t
	return t = (Valor t)


data Fila t = Node t (Fila t)
    | NiulF

criarFila :: Int -> t -> Failable (t, Fila t)
criarFila n firstV = (Valor ((), Node firstV NiulF))