import Data.Char


harEl::(t->Bool)->[t]->Bool
harEl pr [] = False
harEl pr xs = or[pr x | x<-xs]

el::(t->Bool)->[t]->t
el pr (x:xs) = if(pr x) then x else el pr xs

gRep::(t->Bool)->t->[t]->[t]
gRep pr y [] = []
gRep pr y (x:xs) = if(pr x) then y : gRep pr y xs else x : gRep pr y xs

data BT = B Int | N BT Int BT

elt::BT->Int->Bool
elt (B x) t = x == t
elt (N t1 x t2) t= t==x || elt t1 t || elt t2 t 

toL::BT->[Int]
toL (B x) = x : []
toL (N t1 x t2) = x : [] ++ toL t1 ++ toL t2

dup::BT->Bool
dup tr = dupHelp(toL tr)

dupHelp [] = False
dupHelp (x:xs) = if elem x xs then True else dupHelp xs

-----2
naboL xs = naboLHelp([(x,[y]) | (x,y) <- xs])

naboLHelp [] = []
naboLHelp (x:xs) = (fst x, snd x :[snd y | y <- xs, elem (fst x) [(fst y)] ]) : naboLHelp (filter (\y-> not (fst x == fst y))xs) 

--kantL::[(t,[t])]->[(t,t)]
--kantL xs = [if length y > 1 then () else (x,y)| (x,[y]) <- xs]

kantL :: [(t,[t1])] -> [(t,t1)]
kantL xs = [(x,ny) | (x,y) <- xs, ny <- y]

--naboer :: [(t,t)] -> t ->[t]
naboer nL x = [snd n | n<-nL, x == (fst n)]

naboer2 kl n = [snd y | y <- filter(\x -> fst x == n) kl]


kL = [('a', 'b'),('a', 'c'),('b', 'd'),('c', 'b'),('d', 'c'),('d', 'e')]

cyc gr n = reverse(cycHelp gr n [n])

cycHelp gr n acc =if (x=="") then [] else if (elem (head x) acc) then (head x) :acc else cycHelp gr (head x) ((head x):acc)
    where x = naboer gr n
















--cyc gr n = cycHelp gr n n

--cycHelp [] _ _ list = list
--cycHelp [] _ _ = []
--cycHelp gr n list = if null (cyc gr n) then list else x : cycHelp (filter(\y-> not (fst y == n))gr) x (x : list)
    --where x = head(cyc gr n)

--cycHelp2 gr n list = reverse(removeDup (n : cycHelp gr n list) [])
--cyc2 gr n list = n : (cycHelp gr n list)
--cyc3 gr n = cyc2 gr n []

--removeDup [] list = []
--removeDup (x:xs) list= if (elem x list) then reverse (x : list) else removeDup xs (x:list)

--cyc::[(t,t)]->t->[t]
--cyc gr n = [snd g | g<-gr, n == fst g] : cyc (filter(\x -> not (fst x == n)) gr) (snd g)
--cyc gr n = [if not (null (snd g)) then head (snd g) : cyc gr (head(snd g)) else snd g | g<-gr, n == fst g]
--cyc gr n = [snd g| g<-gr, n==fst g]

--cycHelp gr n liste= if(cyc gr n == "") then liste else (head(cyc gr n):liste) : (cycHelp gr (head(cyc gr n)) liste)

cycF nL x = let re = trav nL [] x in if null re then [] else reverse (head re)
trav nL vis x = if (elem x vis) then [x:vis] else concat (map (trav nL (x:vis)) (naboer nL x))

---3
main::IO()
main = do
    graf [] ""

graf gr msg= do
    putStrLn(msg)
    input <- getLine
    let inp = words input

    case head inp of
        "g" -> graf [] "Ny tom graf"
        "k" -> if (isDigit x && isDigit y) then graf ((x,y):gr) "Lagt inn x y" else graf gr "ikke lovlig input"
            where 
                x = head (inp !! 1)
                y = head (inp !! 2)
        "f" -> if(isDigit x && isDigit y) then graf (filter(\g -> not ((x,y) == g)) gr) "Fjernet x og y" else graf gr "Ugyldig"
            where 
                x = head (inp !! 1)
                y = head (inp !! 2)
        "s" -> do 
            if (not (null x)) 
                then do putStrLn (head (x))
                        graf gr ""
                else graf gr "Feil"
                where
                    x = [cycF gr (fst n) | n <- gr, not (cycF gr (fst n) == [])]
        "q" -> return()
        
funk::(f -> f -> d)->(f->d)
funk = \h -> \x -> (h x) x

{- 
\h -> \x -> (h x) h
\h -> \x -> (h x) x

-----HM-----
E (Ø                | \h -> \x -> (h x) h ::t) 
(t4) h::a           | \x -> (h x) h ::b)                (t2) (t=a->b)
(t4) h::a, x::c     | (h x) h ::d)                      (t2) (t=a->b, b=c->d)

(t3) h::a, x::c     | (h x) ::e ->d) 
(t2) h::a, x::c     | h :: e

(t2) h::a, x::c     | h :: f -> e -> d
(t2) h::a, x::c     | x :: f 

------MM------
(t=a->b, b=c->d, a=e, a=f->e->d, c=f)
(t=a->b, b=f->d, a=e, a=f->e->d,    c=f)
(t=a->b, b=f->d, f->e->d=e,   a=f->e->d,    c=f)

f->e->d=e-- this will result in check error, and go indefinitaly. 

--------HM------
\h -> \x -> (h x) x
E (Ø                | \h -> \x -> (h x) x ::t)
(t4) (h::a          | \x -> (h x) x ::b)                   (t2)(t=a->b)
(t4) (h::a, x::c    | (h x) x ::d)                         (t2)(t=a->b, b=c->d)

(t3) (h::a, x::c    | (h x) x ::e->d)
(t2) (h::a, x::c    | x ::e)  

(t3) (h::a, x::c    | h ::f ->e ->d)
(t2) (h::a, x::c    | x ::f)  


--MM--
(t=a->b, b=c->d, a=f -> e -> d, c=f, c=e)
(t=a->b, b=e->d, a=f -> e -> d, e=f,    c=e)
(t=a->b, b=f->d, a=f -> f -> d,     e=f,    c=e)
(t=(f -> f -> d)->b, b=f->d,    a=f -> f -> d,     e=f,    c=e)
(t=(f -> f -> d)->(f->d),    b=f->d,    a=f -> f -> d,     e=f,    c=e)











-}














{- 
harEl::(t->Bool)->[t]->Bool
harEl pr [] = True
harEl pr x = or[pr g | g<- x]

el::(t->Bool)->[t]->t
el pr (x:xs) = if(pr x) then x else el pr xs

gRep::(t->Bool)->t->[t]->[t]
gRep pr y [] = []
gRep pr y (x:xs) = if(pr x) then y : gRep pr y xs else x : gRep pr y xs

data BT = B Int | N BT Int BT

elt::BT->Int->Bool
elt (B x) t= x == t 
elt (N t1 x t2) t= t == x || elt t1 t|| elt t2 t

toL::BT->[Int]
toL (B x) = [x] 
toL (N t1 x t2) = (toL t1) ++ [x] ++ (toL t2)

dup::BT->Bool
dup tr = dupH(toL tr)

dupH [] = False
dupH (x:xs) = elem x xs == True || dupH xs

-------2-------
naboL::Eq t => [(t,t)]->[(t,[t])]


-----3-----
main::IO()

-}