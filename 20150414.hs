----------------------------------------------------------
-- Trabalho 7 aluno: Natanael Souza dos Santos login: nss
----------------------------------------------------------

-- Questão 1
compose :: (Num t) => (t -> t) -> [t -> t] -> [t -> t]
compose _ [] = []
compose f (x:xs) = ((f.x ): compose f xs)