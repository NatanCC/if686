----------------------------------------------------------
-- Trabalho 11 aluno: Natanael Souza dos Santos login: nss
----------------------------------------------------------

-- Questão 1
type HashTable = [(Int, Int)]
baseHash :: HashTable
baseHash = [(0,0), (0,0), (0,0), (0, 0), (0, 0), (0, 0), (0, 0), (0, 0)]


data Hash t = Just' t
    | Nothing'
    deriving (Show, Eq)

instance Monad Hash where
	(>>=) (Just' t) f = f t
	(>>=) Nothing' f = Nothing'
	return t = (Just' t)


get :: HashTable -> Int -> (Int, Int)
get h key  = getItem h (hashFunction key)

put :: HashTable -> (Int, Int) -> Hash HashTable
put h (key, val) = (Just' (putItem h (key, val) (hashFunction key)))

remove :: HashTable -> Int -> Hash HashTable
remove h key | hasItem h key (hashFunction key) = (Just' (removeItem h (hashFunction key)))
             | otherwise = Nothing'

hasKey :: HashTable -> Int -> Hash Bool
hasKey h key | hasItem h key (hashFunction key) == True = (Just' True)
             | otherwise = Nothing'

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

main' :: Hash HashTable
main' = do
	a <- put baseHash (5, 2)
	b <- put a (23, 45)
	c <- remove b 5
	d <- put c (87, 32)
	e <- put d (765, 09)
	f <- remove e 87
	g <- put f (1, 2)
	h <- put g (2, 1)
	i <- remove h 1
	j <- put i (90, 90)
	k <- put j (0, 0)
	l <- remove k 0
	m <- put l (12, 13)
	n <- put m (32, 65)
	remove n 32



main :: IO ()
main = do
	putStrLn "Digite uma cadeia de String"
	s <- getLine
	putStrLn s


