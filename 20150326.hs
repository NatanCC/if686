----------------------------------------------------------
-- Trabalho 3 aluno: Natanael Souza dos Santos login: nss
----------------------------------------------------------

-- Questão 1
type HashTable = [(Int, Int)]
baseHash :: HashTable
baseHash = [(0,0), (0,0), (0,0), (0, 0), (0, 0), (0, 0), (0, 0), (0, 0)]

get :: HashTable -> Int -> (Int, Int)
get h key  = getItem h (hashFunction key)

put :: HashTable -> (Int, Int) -> HashTable
put h (key, val) = putItem h (key, val) (hashFunction key)

remove :: HashTable -> Int -> HashTable
remove h key = removeItem h (hashFunction key)

hasKey :: HashTable -> Int -> Bool
hasKey h key = hasItem h key (hashFunction key)

getItem :: HashTable -> Int -> (Int, Int)
getItem [] _ = (0, 0)
getItem (x:h) 0 = x
getItem (x:h) code = getItem h (code-1)

putItem :: HashTable -> (Int, Int) -> Int -> HashTable
putItem (s:h) (key, val) 0 = [(key, val)]++h
putItem (s:h) (key, val) code = [s]++ putItem h (key, val) (code-1)

removeItem :: HashTable -> Int -> HashTable
removeItem (s:h) 0 = [(0, 0)]++h
removeItem (s:h) code = [s]++ removeItem h (code-1)

hasItem :: HashTable -> Int -> Int -> Bool
hasItem ((key, val):s) key1 0
    | key1 == key = True
    | otherwise = False
hasItem (h:s) key code = hasItem s key (code-1)

hashFunction :: Int -> Int
hashFunction x = (5 * x) `mod` 8

-- Questão 2

comparaConjuntos :: (Eq t) => [t] -> [t] -> String
comparaConjuntos a b
    | inter a b == a && inter b a == b = "A igual a B"
    | inter a b == [] = "Conjuntos Disjuntos"
    | uniao a b == b = "B contem A"
    | uniao b a == a = "A contem B"
	| otherwise = "A interseciona B"


member :: (Eq t) => t -> [t] -> Bool
member e [] = False
member e (x:xs) | x == e = True
                | otherwise =  member e xs

inter :: (Eq t) => [t] -> [t] -> [t]
inter [] c = []
inter (x:xs) c
    | member x c = x:(inter xs c)
    | otherwise = inter xs c

uniao :: (Eq t) => [t] -> [t] -> [t]
uniao [] c =  c
uniao (x:xs) c
    | member x c = uniao xs c
    | otherwise = x:(uniao xs c)
