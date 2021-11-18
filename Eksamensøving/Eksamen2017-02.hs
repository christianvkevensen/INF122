{-
lengthsum :: (Num a, Num b) => [a] -> (b, a)
lengthsum = foldr (\n (x,y) -> ((1+x), (n+y))) (0,0)

lengthsum2 xs = (length xs, foldr (+) 0 xs)


inList :: (Eq a) => a -> [a] -> Bool
inList n xs = foldl (\ acc y -> if n == y then True else acc) False xs

-}





lengthsum :: (Num a, Num b) => [a] -> (b, a)
lengthsum = foldr(\n (x,y) -> (x+1, y+n)) (0,0)

inList :: (Eq a) => a -> [a] -> Bool
inList x xs = foldl(\acc n -> if n == x then True else acc) False xs 


data Expr = V Int | M Expr Expr | D Expr Expr

safeDiv :: Int -> Int -> Maybe Int
safeDiv _ 0 = Nothing
safeDiv x y = Just (x `div` y)

eval::Expr -> Maybe Int
eval (V x) = Just x
eval (M x y) = case eval x of
    Nothing -> Nothing
    Just n -> case eval y of
        Nothing -> Nothing
        Just m -> if n >= 0 && m >= 0 then Just (m * n) else Nothing

eval (D x y) = case eval x of
    Nothing -> Nothing
    Just n -> case eval y of
        Nothing -> Nothing
        Just m -> safeDiv n m
--eval (M )

---4

data Month = January  | February | March | April  | Mai  | Juni  | Juli  | August  | September  | Oktober  | November  | Desember  

numDays :: Month -> Integer -> Integer
numDays January x = 31
numDays February x = if (mod x 4== 0) then 29 else 28
numDays March x = 31
numDays April x = 31
numDays Mai x = 31
numDays Juni x = 31
numDays Juli x = 31
numDays August x = 31
numDays September x = 31
numDays Oktober x = 31
numDays November x = 31
numDays Desember x = 31

---5
toDoList :: [IO a] -> IO [a]
toDoList [] = return []
toDoList (a:as) = do
    v <- a
    vs <- toDoList as
    return (v:vs)

--mapActions :: (a -> IO b) -> [a] -> IO [b]
func::((e->d)->e)->(((e->d)->e)->(e->d))->d
func = \ x -> \ y -> (y x) (x (y x))
---6
{-
------HM------
\ x -> \ y -> (y x) (x (y x))

E Ø                | \ x -> \ y -> (y x) (x (y x)) :: t
(t4)x :: a         | \ y -> (y x) (x (y x)) :: b                       (t2)(t = a->b)
(t4)x :: a, y :: c | (y x) (x (y x)) :: d                              (t2)(t = a->b, b = c->d)  

(t3)x::a, y::c     | (y x) :: e -> d
(t2)x::a, y::c     | y :: f ->(e -> d)
(t2)x::a, y::c     | x :: f

(t3)x::a, y::c     | x (y x)) :: e
(t2)x::a, y::c     | x :: g ->e
(t2)x::a, y::c     | y x :: g
(t2)x::a, y::c     | y :: h -> g
(t2)x::a, y::c     | x :: h

-----MM----
(t= a->b, b=c->d, c=f->(e->d), a=g->e, c=h->g,a=f,a=h)
(t= h->b, b=c->d, c=f->(e->d), h=g->e, c=h->g,h=f)
(t= f->b, b=c->d, c=f->(e->d), f=g->e, c=f->g)
(t= (g->e)->b, b=((g->e)->g)->d, g=(e->d))
(t= (((e->d))->e)->b, b=((((e->d))->e)->((e->d)))->d)
(t= ((e->d))->e)->(((e->d))->e)->e->d)->d)


























-}