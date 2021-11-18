--------1-10-------
myLast list = last list

myButLast list = last (init list)
myButLast2 = last . init

elementAt list x = list !! (x-1)

myLength list = length list

myReverse list = reverse list

isPalindrom list = reverse list == list

data NestedList a = Elem a | List [NestedList a]
flatten (Elem a) = [a]
flatten (List x) = concatMap flatten x

compress list = compress_ list ' '
compress_ [] _= []
compress_ (x:xs) n = if (x == n) then compress_ xs n else x:compress_ xs x

compressF list = reverse(foldl(\acc y -> if(acc == "") then y:acc else if (head acc == y) then acc else y:acc) "" list)

pack list = foldr(\x acc -> if acc == [[]] then [x]:acc else if(head(head acc) == x) then [x:head(acc)] ++ (tail acc) else [x]:acc)[[]] list


encode liste = [(head x,length x) | x <- pack liste, x/=""]
encode2 liste = map(\x -> (x, length x)) (pack liste)


----------------11-20--------------
data List x = Single x | Multiple Int x
    deriving(Show)

encodedModified list = [if snd x > 1 then Multiple (snd x) (fst x) else Single (fst x) | x<- list_]
    where list_ = encode list


--encodedModifiedHelper (Single x) = x
--encodedModifiedHelper (Multiply x y) = x y 

--dupli liste = foldl(\acc x -> x : acc)
--dropEveryH x n = dropEveryH x n 1 
dropEvery xs n = dropEveryH xs n 1 

dropEveryH [] _ _ = []
dropEveryH (x:xs) n y= if (mod y n == 0) then dropEveryH xs n (y+1) else x : dropEveryH xs n (y+1)

plus = (*)
leggInn = (:)

split liste n= splitAt n liste

slice liste n m = take (m-n+1) (drop (n-1) liste)  

rotate liste n = snd (split liste n) ++ fst(split liste n)

removeAt n xs = (xs!!(n-1),take (n-1) xs ++ drop n xs)


------21-30-------
insertAt c xs n = take (n-1) xs ++ [c] ++ drop (n-1)xs

range x y = [x..y]

--combinations ys = [y:map (\x->y:combinations (tail ys) )ys  | y <- ys]

{-
--------HM------
E (Ø)                   | \ x -> \ y -> \ z -> (x z) (y z) :t
(t4) x::a               | \ y -> \ z -> (x z) (y z) :b                      {t=a->b}
(t4) x::a, y::c         | \ z -> (x z) (y z) :d                             {t=a->b, b=c->d}
(t4) x::a, y::c, z::e   | (x z) (y z) :f                                    {t=a->b, b=c->d, d=e->f}
(t3) x::a, y::c, z::e   | (x z) : g->f                                      {t=a->b, b=c->d, d=e->f}
(t3) x::a, y::c, z::e   | (y z) : g                                         {t=a->b, b=c->d, d=e->f}

(t2) x::a, y::c, z::e   | x : h->g->f                                       {t=a->b, b=c->d, d=e->f}
(t2) x::a, y::c, z::e   | z : h                                             {t=a->b, b=c->d, d=e->f}

(t2) x::a, y::c, z::e   | y : i->g                                          {t=a->b, b=c->d, d=e->f}
(t2) x::a, y::c, z::e   | z : i                                             {t=a->b, b=c->d, d=e->f}

-----MM------
t=a->b, b=c->d, d=e->f, a=h->g->f, e=h, c=i->g, e=i

e=i
t=a->b, b=c->d, d=i->f, a=h->g->f, i=h, c=i->g 

c=i->g 
t=a->b, b=(i->g)->d, d=i->f, a=h->g->f, i=h

i=h
t=a->b, b=(h->g)->d, d=h->f, a=h->g->f

a=h->g->f
t=(h->g->f)->b, b=(h->g)->d, d=h->f 

d=h->f 
t=(h->g->f)->b, b=(h->g)->(h->f)

b=(h->g)->(h->f)
t=(h->g->f)->((h->g)->(h->f))

-}

funk::(h->g->f)->((h->g)->(h->f))
funk = \ x -> \ y -> \ z -> (x z) (y z)

delM [] = []
delM liste = delMH liste

delMH [] = [[]]
delMH (x:xs) = delMH xs ++ map (x:) (delMH xs)

combinations n xs = filter(\x -> n==length x) (delM xs)


group [a,b,c] xs = group2 a xs : group2 b xs : [group2 c xs]
group2 a xs = [combinations a xs]

rep xs = foldr(\y acc -> if elem y acc then acc else y:acc) [] xs

lsort [] acc = acc
lsort (x:xs) [] = lsort xs [x]
lsort (x:xs) acc= lsort xs (addTo x acc)

addTo x [] = [x]
addTo x (y:ys) = if length x < length y then x:y:ys else y:addTo x ys
---------------------------------

lfsort1 list = lfsort list [] list

lfsort [] acc _= acc
lfsort (x:xs) [] whole = lfsort xs [x] whole
lfsort (x:xs) acc whole= lfsort xs (addTolf x acc whole)whole

addTolf x [] _ = [x]
addTolf x (y:ys) xs =if sum_ x < sum_ y
    then x:y:ys
    else y:addTolf x ys xs
    where
        sum_ e= sum (map (\h -> 1) (filter(\z -> length z == length e) xs))

data Liste = Element Int| ListE [Liste] 
summer xs = sum (sumList xs)

sumList (Element x) = x:[]
sumList (ListE xs) = concat [sumList x | x<- xs]
