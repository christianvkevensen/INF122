--c
--(+) 2:: Num
--(+2) :: Num
--(2+) :: Num
--(["foo", "bar"],'a') :: ([[Char]], Char)
--[(True, []), (False, [['a']])] :: [(Bool, [[Char]])]
-- \x y -> y !! x :: Int -> [a] -> a
-- [ take, drop, \x y -> ( y !! x ) ] :: error
-- [ take, drop, \x y -> [ y !! x ] ] :: [Int -> [a] -> [a]] 

--d
foo1 x y = (x,y) --ekvalient
foo2 x = \y -> (x, y) --ekvalient
foo3 = \x y -> (x, y) --ekvalient
foo4 = \x -> \y -> (x, y) --ekvalient
foo5 = \x -> \y -> (y,x)
foo6 = \y -> \x -> (y,x)--ekvalient

--e
f1 :: a -> (a,a)
f1 a = (a,a)

f2::(a,b) -> a
f2 (a,b) = a

f3::(a,b) -> b
f3 (a,b) = b

f4:: a -> b -> a
f4 a = \b -> a

f5:: a -> b -> b
f5 a = \b -> b 

--f

f :: Int -> Int -> Int
f a = \b -> a+b 

g :: (Int, Int) -> Int
g (a,b) = a+b
