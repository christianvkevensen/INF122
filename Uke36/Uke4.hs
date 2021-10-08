--a
mult :: Int -> Int -> Int -> Int
--mult x y z = x * y * z
mult = \x -> \y -> \ z -> x*y*z

--b
f = \x -> x*x  --Int -> Int
g = \y -> f (f y)  --Int -> Int
h y s = y (y s) -- (t -> t) -> t -> t
--k x = h g x -- Int -> Int
--k :: Int -> Int
--k = \x -> x^16

--C

--s = \ f g x -> f x (g x)
s g f = \x -> x --Forenklet versjon av s
k = \ x y -> x

{- FORKLARING
    s k k = t1 -> t1 (fordi den alltid ignorerer de to første parameterene og vil alltid ta inn t3)
    siden k x y = x
    k x (g x) = x (fordi k x y = x)

    s k k x = x
    s k k = \x -> x
    (for å kjøre) s g f = \x -> x
-}

--d 5.6
--factors :: Int -> [Int]
factors n = sum [x | x <- [1..n], n `mod` x==0, x/= n]

perfects :: Int -> [Int]
perfects n = [x | x <- [1..n], factors x == x]

--5.7
eks = concat [[(x,y) | x <- [1,2]] | y <- [3,4]]

--E
isPrefix :: Eq t => [t] -> [t] -> Bool
isPrefix [] _ = True
isPrefix _ [] = False
isPrefix (x:xs) (y:ys)  | x /= y = False
                        | otherwise = isPrefix xs ys

--F
rem1 :: Eq a => [a] -> a -> [a]
rem1 [] _ = []
rem1 (x:xs) b   | b == x = xs
                | b /= x = x:rem1(xs) b
                | otherwise = []

