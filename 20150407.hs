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
