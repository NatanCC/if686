----------------------------------------------------------
-- Trabalho 10 aluno: Natanael Souza dos Santos login: nss
----------------------------------------------------------

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