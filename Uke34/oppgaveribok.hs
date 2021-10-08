--1.7.1
dobb :: Int -> Int
dobb(x) = 2 * x
--2
summ :: [Int] -> Int
summ [] = 0
summ(x) = sum(x)
--3
productt :: [Int] -> Int
productt [] = 0
productt xs = foldl (*) 1 xs
--4
qsort [] = []
qsort (x:xs) = qsort smaller ++ [x] ++ qsort larger
    where 
        smaller = [a | a <- xs, a >= x]
        larger = [b | b <- xs, b < x]

--2.7.2
--2^3*4 = 2^(3*4)
--(2*3)+(4*5)
--2+(3*(4^5))

n = a `div` (length xs)
    where
        a = 10
        xs = [1,2,3,4,5]
-- feil var parantes, stor n og feil tegn på div

--4
lastt :: [Int] -> Int
lastt [] = 0
lastt(x) = sum(drop((length x)-1) x)
