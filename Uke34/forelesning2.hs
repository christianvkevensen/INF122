qs [] = []
mind x [] = []
mind x (y:ys) = if(x > y) then (y:mind x ys) else (mind x ys)

str x [] = []
str x(y:ys)=if(x<=y) then y:str x ys else str x ys

--qs(x:xs) = qs (mind x xs) ++ [x] ++ qs (str x xs)

--mind x ys = [y | y <- ys, y<x]

--qs (x:xs) = qs[y|y <- xs, y<x] ++ [x] ++ qs [y|y <-xs, y >=x]

rq(x:xs) = rs[y|y <- xs, y>=x] ++ [x] ++ rs [y|y <-xs, y < x]
