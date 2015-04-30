----------------------------------------------------------
-- Trabalho 10 aluno: Natanael Souza dos Santos login: nss
----------------------------------------------------------

foldr (+).(.).map

:t map = (a -> b) -> [a] -> [b]
:t (.) = (u -> v) -> (t -> u) -> (t -> v)

 u = (u' -> v')
 v = (t' -> u') -> (t' -> v')

 (t -> u) -> (t -> v)
 (t -> u' -> v') -> t -> (t' -> u') -> t' -> v'

 t = (a -> b)
 u' -> v' = [a] -> [b]
 u' = [a]
 v' = [b]

 (.).map :: (a -> b) -> (t' -> [a]) -> t' -> [b]

 (+) :: Num x => x -> x -> x

 u = x
 v = x -> x

 (t -> x) -> (t -> x -> x)

 t = (a -> b)
 x = (t' -> [a]) -> t' -> [b]

 (+).(.).map :: Num ((t' -> [a]) -> t' -> [b]) => (a -> b) -> ((t' -> [a]) -> t' -> [b]) -> ((t' -> [a]) -> t' -> [b])

 foldr :: (k -> l -> l) -> l -> [k] -> l

 k = (a -> b)
 l -> l = (t' -> [a]) -> t' -> [b] -> (t' -> [a]) -> t' -> [b])
 l = (t' -> [a]) -> t' [b]

 foldr (+).(.).map :: Num ((t' -> [a]) -> t' -> [b]) => ((t' -> [a]) -> t'-> [b]) -> [(a -> b)] -> ((t' -> [a]) -> t' -> [b])

----------------------------------------------------------------

• (\x y z -> foldr z x y).map
 
---resolvendo foldr e lambd
foldr :: (a->b->b) -> b -> [a] -> b
. (b1->c1) -> (a1-> b1) -> (a1 -> c1)
x y z
z x y
b -> [a] -> (a->b->b) -> b
------com o resultado acima aplica agora a .map
b -> [a] -> (a->b->b) -> b
map (a1->b1) -> [a1] -> [b1]
. (b2->c2) -> (a2-> b2) -> (a2 -> c2)
----------map sera o sgeundo parametro e o nosso (fold lamb) o primeiro
b2 = b
c2 =  [a] -> (a->b->b) -> b
 
a2 = (a1->b1)
b2 =  [a1] -> [b1]
 
b2 =b2
b = [a1] -> [b1]
 
--aplicando o ultimo lado do .
a2 -> c2
(a1->b1) -> [a] -> (a->b->b) -> b
--substituindo pelo tipo mais complexo o b
(a1->b1) -> [a] -> (a->[a1] -> [b1]->[a1] -> [b1]) -> [a1] -> [b1]


-----------------------------------------------------------

map.((.) (foldr (++) (foldr (++) [] [[1], [2]])))

(foldr (++) [] [[1], [2]])

(++) :: [a] -> [a] -> [a]
foldr :: (x -> y -> y) -> y -> [x] -> y

x = [a]
y = [a]

[a] -> [[a]] -> [a]

(foldr (++) [] [[1], [2]]) :: Num a => [a]

(foldr (++) [1, 2])


(++) :: [b] -> [b] -> [b]

z = [b]
w = [b]
 [b] -> [[b]] -> [b]

 Num b => [[b]] -> [b]

 (.) :: (u -> v) -> ( t -> u) -> (t -> v)

 u = [[b]]
 v = [b]

 ((.) (foldr (++) (foldr (++) [] [[1], [2]]))) :: (t -> [[b]]) -> t -> [b]

 map :: (k -> l) -> [k] -> [l]

 u' = (k -> l)
 v' = [k] -> [l]

 (t' -> (k -> l)) -> t' -> [k] -> [l]

 t' = (t -> [[b]])
 k = t
 l = [b]

 Num b => (t -> [[b]]) -> [t] -> [[b]]


---------------------------------------------------------------
 (.) :: (u -> v) -> ( t -> u) -> (t -> v)
(!!) :: [a] -> Int -> a
($) :: (x -> y) -> x -> y
foldr :: (a' -> b' -> b') -> b' -> [a'] -> b'



u = [a]
v = Int -> a

(t -> [a]) -> t -> Int -> a
----------------------------------
(t -> [a]) -> b' -> [a'] -> b'
--------------------------------

u' = (a' -> b' -> b')
v' = b' -> [a'] -> b'

(t' -> a' -> b' -> b') -> (t' -> b' -> [a'] -> b')

t' = (t -> [a])
a' -> b' -> b' = t -> Int -> a







