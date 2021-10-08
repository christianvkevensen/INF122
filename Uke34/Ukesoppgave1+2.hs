--c 
dobb :: [Int] -> [Int]
dobb [] = []
dobb (x : xs) = 2 * x : dobb xs

--d
fire :: [Int] -> [Int]
fire [] = []
fire (x : xs) = 4 * x : fire xs

fire2 :: [Int] -> [Int]
fire2 [] = []
fire2 (x : xs) = if (null xs) then [] else (x*4 : fire2 xs)

--e
flett :: [Int] -> [Int] -> [Int]
flett xs [] = xs
flett [] ys = ys
flett (x:xs)(y:ys) = if x<=y then (x:flett(xs)(y:ys)) else y: flett(x : xs)(ys)


--f
ele :: Int -> [a] -> a
ele a [x] = x
ele a (x : xs) = if a == 1 then x else ele (a - 1) (xs) 

--g
addobb :: [Int] -> [Int]
addobb [] = []
addobb (x : xs) = x:xs ++ dobb(x : xs)

--h
pali [x] = True
pali [] = True
pali (x:xs) = x:xs == (reverse (x:xs))
--pali (x:xs) = last(xs) == x && pali(init(xs))