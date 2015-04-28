----------------------------------------------------------
-- Trabalho 9 aluno: Natanael Souza dos Santos login: nss
----------------------------------------------------------

data Graph t = NilG
    | NodeG t [(t, Int)] (Graph t)
        deriving (Show, Eq)