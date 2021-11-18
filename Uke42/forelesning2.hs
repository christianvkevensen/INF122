--T := variabel | konstant | fapplikasjon

--V := x | y | ...

--K := 12 | (*) ...

--FA := fun (T, ..., T)

--fun := f | ...

--f ( g(x), f(x,n), b)


--------------------------------Substisjon------------------------------------------
--s = {f(x,y), y =}

----------------------------------Unifikasjon----------------------------------------
--en 'forening' som gir syntaktisk likhet 
--alle svar blir like 

-----------------------------Martelli-Montanari---------------------------------------------

--s ( g(a(x,y),x))
-- = g( a(f(x,y),x), f(x,y))

-------------------------------regler-------------------------------------------
-- Fjern x = x og c = c for variabler x og atomer c
-- Snu f (t1,...,tk ) = x til x = f (t1,...,tk ) (inkl. f en konstant)
-- Erstatt f (t1,...,tk ) = f (s1,...,sk ) med t1 = s1,...,tk = sk
-- Hvis det finnes x = t der x ikke forekommer i t (‘occurscheck’), erstatt alle ligninger s = r (untatt x = t selv) med
--s[x/t] = r[x/t].

--NO: hvis du finner en likning der
-- – occurs check feiler: x = t og x forekommer i t, eller
-- – f (...) = g(...) og f =/= g (inkl. konstanter).
--YES: og returner transformerte E, hvis:
--1. i hver likning er venstresiden en variabel, og
--2. hver variabel som forekommer p˚a venstresiden i en ligning ikke
--forekommer noe annet sted

--------eks-------
--{f(t) = x , g(x) = h(y), f(x) = f(y)}
--{x = f(t) , g(x) = h(y), f(x) = f(y)}
--{x = f(t) , g(f(t)) = h(y), f(f(t)) = f(y)}

--------------------------------------------------------------

--1. E,t = t ) =>E
--2. E, f (t1...tn) = f (s1...sn) ) => E,t1 = s1, ...,tn = sn
--3. E, f (t1...tn) = g(s1...sm) )=> NO f =/= g _ n 6= m
--4. E, f (t1...tn) = x )=> E, x = f (t1...tn)
--5. E, x = t ) => E[x/t], x = t  x /e Var(t)
--6. E, x = t )=> NO x e Var(t)

p(X,X) = p(Y, f(Y))
    -> {X = Y, X = f(Y)} -> {X = Y, Y = f(Y)} -- occurs check



--------------------Typeinferens-------------------------------
{-

expr        ::= \exprvar -> expr | funapp
funapp      ::= funapp simpexpr | simpexpr | (*) | ...
simpexpr    ::= exprcon | exprvar | (expr)
exprcon     ::= 0 | 1 | ... | 3.14 | ... | True | False
exprvar     ::= lower case letter
type        ::= type’ -> type | type’
type’       ::= typecon | typevar | (type)
typecon     ::= Int | Bool | ...
typevar     ::= lower case letter
typeassignment ::= environment | exprvar :: type
environment    ::= { typeassignment }

f.eks. \x -> (*) x 2 :: Int -> Int
(\x -> (*) x 2) 6 :: Int
\x -> \y -> x y :: (a -> b) -> a -> b

-}

-----------------regler------------------------
{-

(t1) E(T | con :: t) = {t = 0(con)}
(t2) E(T | x :: t) = {t = T(x)}
(t3) E(T | f g :: t) = E(T | g : a) [ E(T | f :: a -> t)
(t4) E(T | \x -> ex :: t) = {t = a -> b} [ E(T, x :: a | ex :: b)
– a,b er ferske

-}

--------------Hindley-Milner-----------------------
{-
E(0 | \x -> x::t) = 
    (t4){t = ->}U E(x :: a | x :: b) = 
    (t2){t = a ->b}U {b = a} =
    (MM){t = a -> b, b=a}
    -unifiserer-
    {t = a -> b, b=a}
    dvs svaret er \x -> x :: a -> a
}