import System.IO
import System.Random
import Data.Char
----1


----2
append :: [a] -> [a] -> [a]
append xs ys = foldr(\x y -> x : y) ys xs

type Name = String
type Pnum = Integer
type Pbook = Name -> Maybe Pnum

----3
lookup :: Pbook -> Name -> Maybe Pnum
lookup pb navn = pb navn

--insert :: Pbook -> Name -> Pnum -> Pbook
--insert pb name num = if(feilmelding num) then pb name num

feilmelding nmr = if (length (show nmr) /= 8) then False else True

delete :: Pbook -> Name -> Pbook 
delete pb n = \x -> if x == n then Nothing else pb x

-----4
--main = do
    --gen <- newStdGen
    --let rand = randomRIO(10,20) gen
    --play rand 1 ""

play heap player msg = do
    if (heap == 0) then do
        putStrLn ("Player " ++ show (player) ++ " takes all the rest of heaps")
        else do
            putStrLn(msg)
            putStrLn("Heap size " ++ show(heap) ++ " Player " ++ show(player) ++ " remove 1 to 3 objects from heap")
            input <- getLine
            if (input /="" && isDigit(head (input))) then 
                if (((read(input)::Int) > 0 && (read(input)::Int) <4))
                    then do
                        let x = read(input)::Int
                            nextPlayer = nplayer player
                        if(heap - x >= 0) then
                            play (heap-x) nextPlayer ""
                            else 
                                play 0 nextPlayer ""
                    else play heap player "Wrong input"

                else
                    play heap player "Wrong input"

nplayer n = if n==1 then 2 else 1

funk::(h->g->f)->((h->g)->(h->f))
funk = \ x -> \ y -> \ z -> (x z) (y z)
-----5
{-
-----HM----
E (Ø)               | \ x -> \ y -> \ z -> (x z) (y z) :t
(t4) x::a           | \ y -> \ z -> (x z) (y z) :b                       (t2) (t=a->b)
(t4) x::a, y::c     | \ z -> (x z) (y z) :d                              (t2) (t=a->b, b=c->d)
(t4) x::a, y::c,z::e| (x z) (y z) :f                                     (t2) (t=a->b, b=c->d, d=e->f)

(t3) x::a, y::c,z::e| (x z) :g->f
(t3) x::a, y::c,z::e| x :h->g->f
(t3) x::a, y::c,z::e| z :h

(t3) x::a, y::c,z::e| (y z) : g
(t3) x::a, y::c,z::e| y : i->g
(t3) x::a, y::c,z::e| z : i


-----MM-----
(t=a->b, b=c->d, d=e->f, a =h->g->f, e=h, c=i->g, e=i)
(t=a->b, b=c->d, d=i->f, a =h->g->f, i=h, c=i->g,   e=i)
(t=a->b, b=(i->g)->d, d=i->f, a =h->g->f, i=h,   c=i->g,   e=i)
(t=a->b, b=(h->g)->d, d=h->f, a =h->g->f,   i=h,   c=i->g,   e=i)
(t=(h->g->f)->b, b=(h->g)->d, d=h->f,   a =h->g->f,   i=h,   c=i->g,   e=i)
(t=(h->g->f)->b, b=(h->g)->(h->f),   d=h->f,   a =h->g->f,   i=h,   c=i->g,   e=i)
(t=(h->g->f)->((h->g)->(h->f)),    b=(h->g)->(h->f),   d=h->f,   a =h->g->f,   i=h,   c=i->g,   e=i)



-}






