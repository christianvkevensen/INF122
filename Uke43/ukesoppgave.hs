--1
{-
(t1) E(T | con :: t) = {t = 0(con)}
(t2) E(T | x :: t) = {t = T(x)}
(t3) E(T | f g :: t) = E(T | g : a) [ E(T | f :: a -> t)
(t4) E(T | \x -> ex :: t) = {t = a -> b} [ E(T, x :: a | ex :: b)

x y = x
\x -> y -> x

E(0 | \x -> \ y -> x :: t) = 
(t4) x::a | \y -> x :: b ... {t = a->b}
(t4) x::a, y::c | x::b 

b = c-> b, t = a -> b

-}
pro x y = x
apply = \f -> \x -> f x
comp f g x = f(g x)
oppg4 = \x -> \y -> x y
oppg6 = \x -> \y -> x (y x)
--oppg7 = \x -> \y -> x y x
--oppg8 = \x -> x (\y -> x y)
oppg9= \x -> \y -> \z -> (x z) (y z)

{- 
-------HM-pro---------

E(0 | \x -> \ y -> x)
(t4) (x :: a | \y -> x :: b)         {t = a -> b}
(t4) (x :: a, y:: c | x :: d)        {t = a -> b, b = c->d}
t = a -> b, b = c->d, a = d

--------MM-pro---------
t = a -> b, b = c->d, a = d
pro :: d -> c -> d


--2


--Oppgave 3
comp f g x = f(g x)
comp = \f -> \ g -> \ x -> f (g x)

---HM----
E(0 | \f -> \ g -> \ x -> f (g x) :: t)
(t4) (f::a | \ g -> \ x -> f (g x) :: b)    (t = a -> b)
(t4) (f::a, g::c | \ x -> f (g x) :: d)     (t = a->b, b = c->d)
(t4) (f::a, g::c, x::e | f (g x) :: h)      (t = a->b, b = c->d, d = e->h)
(t3) (f::a, g::c, x::e | f :: i -> h)
U (f::a, g::c, x::e | g x :: i)
U (f::a, g::c, x::e | g :: k -> i)
U (f::a, g::c, x::e | x :: k)               (t = a->b, b = c->d, d = e->h)     

--MM--
t = a->b, b = c->d, d = e->h, a = i -> h, c = k -> i,    e = k
t = a->b, b = c->d, d = k->h, a = i -> h,   c = k -> i, e = k
t = a->b, b = (k->i)->d, d = k->h,       a = i -> h, c = k -> i, e = k
t = (i->h)->b, b = (k->i)->d, d = k->h,       a = i -> h, c = k -> i, e = k
t = (i->h)->b, b = (k->i)->(k->h),       d = k->h, a = i -> h, c = k -> i, e = k
t = (i->h)->(k->i)->(k->h),     b = (k->i)->(k->h), d = k->h, a = i -> h, c = k -> i, e = k

comp :: (i->h)->(k->i)->(k->h)


-------Oppgave 4-9---------
------------HM-------------
4. \x -> \y -> x y
E(0 | \x -> \y -> x y :: t)
(t4) (x :: a | \y -> x y :: b)                  (t = a->b)
(t4) (x :: a, y :: c | x y :: d)                (t = a->b, b = c->d)
(t3) (x :: a, y :: c | x :: e->d)               (t = a->b, b = c->d)
(t3) (x :: a, y :: c | y :: e)                  (t = a->b, b = c->d)

-------------------MM----------------
t = a->b, b = c->d, a = e->d, c = e
t = a->b, b = e->d, a = e->d,   c = e
t = (e->d)->b, b = e->d,     a = e->d, c = e
t = (e->d)->(e->d),  b = e->d, a = e->d, c = e
oppg4::(e->d)->(e->d)


5. \x -> \y -> (x y) x
--------HM-------
E(0|\x -> \y -> (x y) x :: t)
(t4)(x::a | \y -> (x y) x :: b)                     (t = a->b)
(t4)(x::a, y::c | (x y) x :: b)                     (t = a->b, b = c->d)
(t4)(x::a, y::c | x :: e)                           (t = a->b, b = c->d)
(t3)(x::a, y::c | x y :: e->d)                      (t = a->b, b = c->d)
U (x::a, y::c   | x :: g->e->d)                     (t = a->b, b = c->d)

-----------------MM-------------
t = a->b, b = c->d,a = g->e->d ,c = g,  a = e
t = e->b, b = c->d,e = g->e->d ,    c = g, a = e
t = e->b, b = g->d,e = g->e->d , c = g, a = e
t = e->b, b = g->d,e = g->e->d , c = g, a = e       occurence check for e = g->e->d



6. \x -> \y -> x (y x)
----------HM---------------
E(0 | T | \x -> \y -> x (y x) ::t)              
(x :: a | \y -> x (y x) :: b)               (t=a->b)
(x :: a, y :: c |  x (y x) :: d)            (t=a->b, b = c -> d)
(x :: a, y :: c |  x :: e -> d) 
(x :: a, y :: c |  (y x) :: e) 
(x :: a, y :: c |  y :: g->e)
U (x :: a, y :: c |  x :: g)                (t=a->b, b = c -> d)

------------------------MM---------------------
t=a->b, b = c -> d, a = e->d, c = g->e,     a = g
t=g->b, b = c -> d, g = e->d,   c = g->e,     a = g
t=g->b, b = g->e -> d, g = e->d,   c = g->e,     a = g
t=(e->d)->b, b = (e->d)->e -> d,  g = e->d,   c = g->e,     a = g
t=(e->d)->((e->d)->) -> d,    b = (e->d)->e -> d,  g = e->d,   c = g->e,     a = g
oppg6 :: (e->d)->((e->d)->e) -> d


t=g->b, b = c -> d, g = e->f,       c = g->e,     a = g
t=g->b, b = (g->e) -> d, g = e->f,       c = g->e,     a = g
t=(e->f)->b, b = ((e->f)->e) -> d,    g = e->f,       c = g->e,     a = g
t=(e->f)->((e->f)->e) -> d,    b = ((e->f)->e) -> d,    g = e->f,       c = g->e,     a = g
oppg6 :: (e->f)->((e->f)->e) -> d




7. \x -> \y -> x y x

-------------HM-----------
E(0 | T | \x -> \y -> x y x :: t)
(t4) (x :: a | \y -> x y x :: b)        (t=a->b)
(t4) (x :: a, y :: c | x y :: d)      (t=a->b, b=c->d) 
(t3) (x :: a, y :: c | x :: e->d)
(t3) (x :: a, y :: c | x:: e)
(t3) (x :: a, y :: c | x:: f->e)
(t3) (x :: a, y :: c | y :: f)

--------------MM-------------
t=a->b, b=c->d, a = e->d, a = e, a = f->e, c=f
t=a->b, b=f->d, a = e->d, a = e, a = f->e,  c=f
t=e->b, b=f->d, e = e->d, e = f->e,     a = e,  c=f     check occurs, error




8. \x -> x (\y -> x y)
----------------------HM------------------------
E(0|T | \x -> x (\y -> x y) :: t)
(t4) (x :: a | x (\y -> x y) :: b)      (t = a->b)
(t3) (x :: a | x :: c->b)               (t = a->b) --check occurs
(t3) (x :: a | \y -> x y :: c)
(t4) (x :: a, y :: d | x y :: e)        (t=a->b, c=d->e)
(t3) (x :: a, y :: d | x :: f->e)
(t3) (x :: a, y :: d | y :: f)




9. \x -> \y -> \z -> (x z) (y z)

---------------HM--------------
E(0|T | \x -> \y -> \z -> (x z) (y z) :: t)
(t4) (x :: a | \y -> \z -> (x z) (y z) :: b)            (t=a->b)
(t4) (x :: a, y :: c| \z -> (x z) (y z) :: d)           (t=a->b, b=c->d)
(t4) (x :: a, y :: c, z::e| (x z) (y z) :: f)           (t=a->b, b=c->d, d=e->f)
(t3) (x :: a, y :: c, z::e| (x z) :: g->f) 
(t3) (x :: a, y :: c, z::e| (x z) :: h) 
(t3) (x :: a, y :: c, z::e| x :: i->h) 
(t3) (x :: a, y :: c, z::e| z :: i)
(t3) (x :: a, y :: c, z::e| (y z) :: g)
(t3) (x :: a, y :: c, z::e| y :: j->g)
(t3) (x :: a, y :: c, z::e| z :: j)


---------------MM-----------------




-------HM-apply------
E(0 | \f -> \x -> f x :: t)
(t4) (f :: s | x-> f x :: b)        {t = s -> b}
(t4) (f :: s, x :: a | f x :: c)    {t = s -> b, b = a -> c} 
(t3) (f :: s, x :: a | f :: d ->c)
U (f :: s, x :: a | x :: d)         {t = s -> b,b = a -> c, s = d-> c, d = a}

-------MM----------
t = s -> b,b = a -> c, s = d-> c, d = a
t = s -> b,b = a -> c, s = a-> c, d = a
t = (a->c) -> (a->c), b = a -> c, s = a-> c, d = a
apply :: (a->c) -> a -> c



-}
