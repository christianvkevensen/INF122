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