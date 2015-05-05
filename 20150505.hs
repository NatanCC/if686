----------------------------------------------------------
-- Trabalho 11 aluno: Natanael Souza dos Santos login: nss
----------------------------------------------------------
import Data.Char
import Data.Maybe
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


get :: HashTable -> Int -> Hash 	(Int, Int)
get h key | hasItem h key (hashFunction key) = (Just' (getItem h (hashFunction key)))
          | otherwise = Nothing'

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


getInfi1 :: IO ()
getInfi1 = do {
    line <- getLine;
    x <- return $ codefi line;
    if x /= Nothing
        then lerString1 (splitWords (caseMaior (fromJust x)))
        else putStrLn "Nothing"	   
        
}    


getInfi :: IO ()
getInfi =
	do {
		line <- getLine;
		putStrLn $ case codefi line of
			Nothing -> "Nothing"
			Just x ->  x
		 
	}


codefi :: String  -> Maybe String
codefi [] = Nothing
codefi l 
	| config l = Just l
	| otherwise = Nothing


config :: String -> Bool
config [] = True
config (x:xs) 
	| check x ([1..26] ++ (map (negate) [6..31] )) = config xs
	| otherwise = False



check ::  Char  -> [Int] -> Bool
check _ [] = False
check ' ' _ = True
check x  (y:ys) 
	| position x == y = True
	| otherwise = check x ys 
	



position  :: Char -> Int
position a = (ord a) - 96

caseMaior:: String -> String
caseMaior [] = []
caseMaior (x:xs)
	| maior x [1..26] || x == ' ' = [toUpper x] ++ caseMaior xs
	| otherwise = caseMaior xs


maior :: Char -> [Int] ->Bool
maior ' ' _ = False
maior x (y:ys)
	| position x == y = True
	| otherwise = maior x ys

checkespaco :: Char ->	Bool
checkespaco ' '= True


getWord :: String -> String
getWord [] = []
getWord (x:xs) 
	| x /= ' ' = x: getWord xs
	|otherwise = []

dropWord :: String -> String
dropWord [] = []
dropWord (x:xs) 
	| x /= ' ' = dropWord xs
	| otherwise = xs

dropSpace :: String -> String
dropSpace [] = []
dropSpace (x:xs) 
	| x == ' ' = dropSpace xs
	| otherwise = (x:xs)


splitWords :: String -> [String]
splitWords [] = []
splitWords l  = [getWord l ] ++ splitWords (dropSpace (dropWord l))

lerString1 :: [String] -> IO ()
lerString1 [] = putStrLn []
lerString1 (x:xs) = 
	do {
		putStrLn x;
		lerString1 xs;
	}


