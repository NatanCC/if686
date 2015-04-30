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

----------------------------------------------------------
-- Exercício da aula
----------------------------------------------------------

f' :: (t -> u -> v) -> (u -> t -> v)
f' f = (\a b -> f b a)

parList :: [(u, v)] -> [u]
parList l = map (\(a, b) -> a) l

listList l n = filter (\a -> ((length a) > n)) l

listList' :: (Eq t) => [[t]] -> [t]
listList' l = removeDup (foldr (\list -> (list++)) [] l)

removeDup :: (Eq t) => [t] -> [t]
removeDup [] = []
removeDup (x:xs)
    | x `elem` xs = removeDup xs
    | otherwise = x:removeDup xs

--mapfold :: (a1 -> a -> a) -> [a] -> [[a1] -> a]
{- 

((u -> v) -> [u] -> [v]) -> ((a -> b -> b) -> b -> [a] -> b) -> (a -> b -> b) -> [u] -> [v]

-}

sumCons :: Int -> [Int] -> [Int]
sumCons x = map (+x)

listGreater :: [Int] -> Int
listGreater = foldr (>) 0


{- Determine, sem usar o GHCi, os tipos das seguintes expressões:

	foldr (:)

	(:) :: a -> [a] -> [a]
	foldr :: (k -> l -> l) -> l -> [k] -> l

	k = a
	l = [a]

	(foldr (:)) :: [a] -> [a] -> [a]

	-----------------------------------
	
	(.) :: (b -> c) -> (a -> b) -> a -> c

    b = (b' -> c')
    c = (a' -> b') -> a' -> c'
    a = (b'' -> c'')
    b' = (a'' -> b'')
    c' = a'' -> c''

    (.).(.) :: (b'' -> c'') -> (a' -> a'' -> b'') -> a' -> a'' -> c''

	(.).(.) ::
 -}




