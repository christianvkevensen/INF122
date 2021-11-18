import System.IO()

mengde::Eq t => [t] -> Bool
mengde [] = True
mengde (x:xs) = not (elem x xs) && mengde xs

rep::Eq t => [t]->[t]
rep [] = []
rep (x:xs) = if(not (elem x xs)) then x: rep xs else rep xs

del::Eq t => [t]->[t]->Bool
del [] ys = True
del (x:xs) ys = elem x ys && del xs ys

eq::Eq t => [t]->[t]->Bool 
eq xs ys = del xs ys && del ys xs

eqG::(t->t->Bool)->[t]->[t]->Bool 
eqG pr xs ys = delM pr xs ys && delM pr ys xs

delM pr [] ys = True
delM pr (x:xs) ys = or[pr x y | y <-ys] && delM pr xs ys

ps [] = []
ps liste = psHelp(rep liste)

psHelp [] = [[]]
psHelp (x:xs) = psHelp xs ++ map (x:) (psHelp xs)


------2
snuK::[(t,t)]->[(t,t)] 
snuK xs = [(y,x)|(x,y)<- xs]

snuN xs = snuNHelp[(ny,[x]) |(x,y)<- xs, ny <- y]
snuNHelp [] = []
snuNHelp (x:xs) = (fst x, snd x :[ snd res | res <- xs , elem (fst x) [(fst res)] ]) : snuNHelp (filter (\y -> not (fst y == fst x)) xs)

--reach::[(t,[t])]->t->[t]
--reach gr x = rep (reachHelp gr x)

--reachHelp [] x = []
--reachHelp (g:gr) x = if fst g == x then [reach gr y | y <- (snd g)] : reachHelp gr x else reachHelp gr x


--[('a',['c']),('b',['a']),('c',['b']),('d',['c','e']),('e',[])]
-- [(a,[b]),(b,[c]),(c,[a,d]),(e,[d])].

---3
main::IO()
main = do
    input <- getLine
    let inp = words input
    if(null inp) then return() 
    else if(head input == 'L' && length inp < 1)
        then do
        fil <- readFile (head(tail inp))
        if (mengde fil) then do 
            mapM_ putStrLn(ps (fil))
            main 
            else main 
        else do
            mapM_ putStrLn(ps (input)) 
            main

    
----4------
--Resonnering om programmer


























{-
mengde::Eq t => [t] -> Bool
mengde [] = True
mengde (x:xs) = not (elem x xs) && mengde xs

rep::Eq t => [t]->[t]
rep [] = []
rep (m:ms) = if(not(elem m ms)) then m:rep ms else rep ms

del::Eq t => [t]->[t]->Bool 
del [] y = True
del (x:xs) y = elem x y && del xs y

eq::Eq t => [t]->[t]->Bool
eq [] y = True
eq x [] = True
eq (x:xs) (y:ys) = del xs ys && del ys xs

eqG::(t->t->Bool)->[t]->[t]->Bool
eqG pr xs ys = delG pr xs ys && delG pr ys xs

delG pr [] ys = False
delG pr (x:xs) ys = or [pr x y | y <- ys] || delG pr xs ys


ps xs = pl (rep xs)

pl [] = [[]]
pl (x:xs) = [ s | r <- pl xs, s <- [x:r,r] ]



snuK::[(t,t)]->[(t,t)]
snuK [] = []
snuK liste = [(y,x) | (x,y) <- liste]

main::IO() 
main = do
    putStr"liste / CR: "
    input <- getLine
    let c = words input
    if(null c) then 
        return()
        else if (head c == "L")
            then do
                fil <- readFile (head (tail c))
                vis fil
                main
                else do
                    vis c
                    main
            
vis x = if (mengde x) then skriv (ps x) else putStrLn "Flere duplikater, feil"

skriv xs = mapM_ putStrLn (map show xs)


--reach::[(t,[t])]->t->[t]












mengde :: Eq t => [t] -> Bool
mengde [] = True
mengde (x:xs) = not (elem x xs) && mengde xs

rep :: Eq t => [t] -> [t]
rep [] = []
rep (x:xs) = if(elem x xs) then rep xs else x : rep xs

del :: Eq t => [t] -> [t] -> Bool
del [] _ = True
del (x:xs) y = elem x y && del xs y

eq :: Eq t => [t] -> [t]-> Bool
eq [] _ = True
eq _ [] = True
eq xs ys = del xs ys && del ys xs

eqG::(t->t->Bool)->[t]->[t]->Bool
eqG pr [] ys = True
eqG pr (x:xs) ys = or[pr x y | y<- ys] && eqG pr xs ys

pl::[t]->[[t]]
pl[] = [[]]
pl (x:xs) = [s | r <- pl xs, s <- [x:r,r]]
ps xs = pl (rep xs)

-}