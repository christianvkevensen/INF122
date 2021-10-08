--A
remg :: [a] -> (a -> Bool) -> [a]
remg [] _ = []
remg (a:as) b = if head(map b (a:[])) then as else a: remg as b
--7.1
--Show how the list comprehension [f x | x <- xs, p x] can be re-expressed
--using the higher-order functions map and filter.
listComp f p xs = map f (filter p xs)
--7.4
dec2int xs = foldl (\x y -> 10 * x + y) 0 xs

--7.5
-- curry :: ((a,b)->c) -> a -> b -> c

--7.9
altMap :: (a -> b) -> (a -> b) -> [a] -> [b]
altMap _ _ [] = []
altMap a b c = altMapFirst a b c

altMapFirst _ _ []= []
altMapFirst a b (c:cs) = (a c): altMapSecond a b cs

altMapSecond _ _ [] = []
altMapSecond a b (c:cs) = (b c): altMapFirst a b cs