mergeSort :: [Int] -> [Int]
mergeSort [x] = [x]
mergeSort a = merge (mergeSort (divideLeft a 0 ((count a)`div`2))) (mergeSort (divideRight a 0 ((count a) `div`2)))



count :: [Int] -> Int
count [] = 0
count (x:xs) = 1 + count xs

divideLeft :: [Int] -> Int -> Int -> [Int]
divideLeft (x:xs) a b | a < b = (x:(divideLeft xs (a+1) b))
                      | otherwise = []

divideRight :: [Int] -> Int -> Int -> [Int]
divideRight [] a b = []
divideRight (x:xs) a b | a > 2*b = []
                        | a >= b = (x:(divideRight xs (a+1) b))
                        |otherwise = divideRight xs (a+1) b

merge :: [Int] -> [Int] -> [Int]
merge [] [] = []
merge (a:as) (b:bs) | a <= b = (a: merge as (b:bs))
                    | otherwise = (b: merge (a:as) bs)
merge (a:as) [] = (a: merge as [])
merge [] (b:bs) = (b: merge [] bs)
