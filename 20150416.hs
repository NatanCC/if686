----------------------------------------------------------
-- Trabalho 8 aluno: Natanael Souza dos Santos login: nss
----------------------------------------------------------

listPartitioner :: (Num a, Ord a) => [a] -> ([a] -> [[a]])
listPartitioner l = (\sl -> part (sort l) sl)
    where
    	sort [] = []
    	sort  (x:xs) = sort [a | a <- xs, a<=x] ++ [x] ++ sort [a | a <- xs, a > x]
    	part [] sl = [sl]
    	part (a:fl) sl = (filter (<=a) sl): part fl (filter (>a) sl)