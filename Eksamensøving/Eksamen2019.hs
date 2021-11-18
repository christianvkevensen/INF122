import Data.Char
----Oppg 1----
{-

-}

----Oppg 2----
row::[[Int]] -> Int -> [Int]
row m r = m !! (r-1)

col::[[Int]] -> Int -> [Int] 
col m k = [x !! (k-1) | x<-m]

cols:: [[Int]]-> [[Int]]
cols m = [col m k | k<- [1.. length m]]

--mult::[[Int]]->[[Int]]->[[Int]]

----Oppg3-----

eval::IO()
eval = do
    utregn [] ""

utregn liste msg = do
    putStr msg
    putStrLn (show liste)
    input <-getLine
    let inp = words input
    if inp == [] then return ()

    else if (and[isDigit x | x <- head inp]) then do
        let x = read(head(inp))::Int
        utregn (x:liste) ""

    else if(elem (head (inp)) ["+", "-", "*"] && length liste >1) 
        then do
            let x = head liste
                y = head (tail (liste))
                nyL = tail (tail liste) 
            r = case (head inp) of
                "+" -> (x + y) 
                "-" -> (x-y)
                "*" -> (x*y)
            utregn (r:liste) ""
        else utregn liste "feil"
              

            



























{-
row::[[Int]] -> Int -> [Int]
row m r = m !! (r-1)

col::[[Int]] -> Int -> [Int]
col m k = [x !! (k-1) | x <- m]

cols::[[Int]] -> [[Int]]
cols m = [col m k | k <- [1..(length m)]]

eval::IO()
eval = do
    regn [] ""

regn liste mld= do
    putStrLn(mld)
    putStrLn(show liste)
    ls <- getLine
    let input = filter (/= ' ') ls
    if (input == "")then
        return()
        else if(isDigit (head input))
            then do
                let nyL = (read input :: Int) : liste
                regn nyL ""
                else if(elem(head input) ['+', '-', '*'] && length liste > 1)
                    then do
                        let nyListe = tail (tail liste)
                            h = head liste
                            t = head(tail liste)
                            r = case (head input) of 
                                '+' -> (h + t)
                                '-' -> (t - h)
                                '*' -> (h * t)
                        regn (r:nyListe) ""
                    else regn liste "Feil"
            
                
{-
regn liste mld= do
    putStr(mld)
    putStrLn(liste)
    ls <- getLine
    let input = filter (/= ' ') ls
    if (input == "")then
        return()
        else
            if(isDigit (head input))
                then do
                    let nyL = (read input :: Int) : liste
                    regn nyL ""
            else if(elem(head input) ['+', '-', '*'])
                then do
                    let nyListe = tail (tail input)
                    let h = read(head liste)::Int
                    let t = read(head(tail input))::Int
                    case (head input) of 
                        '+' -> (h + t) : nyListe
                        '-' -> (h - t) : nyListe
                        '*' -> (h * t) : nyListe
                    regn nyListe ""
            else if (length input < 2)
                then regn liste "Mangler et argument"
                else 
                regn liste "Feil input"
-}

{- 
Evaluering av filter even (map (*2) [1..5]) gir:
[2,4,6,8,10]

1.2
take 5 nats
a) [0,1,1,1,1]
b) [0, tail nats terminerer ikke
c) [0,1,2,3,4]
d) [1,2,3,4,5]

1.3
concat ["ab","cd","","efg"]
a) concat xss = [x | x<-xss] = ["ab","cd","","efg"] 
b) concat xss = [x | xs<-xss, x<-xs] = "abcdefg"
c) concat xss = concat (tail xss) = feiler, vil prøve å taile en tom liste tilslutt
d) concat xss = map (++) xss = vil (++) på hvert element, vil feile

1.4
apply f x = f x
E(0 | T | \f -> \x = f x ::t)
(t4)(f::a | \x = f x ::b)       (t = a ->b)
(t4)(f::a, x :: c | f x ::d)    (t2)(t = a ->b, b = c ->d)
(t3)(f::a, x :: c | f ::e ->d)
(f::a, x :: c | x ::e)

-------MM-----
t = a ->b, b = c ->d, a = e->d, c= e
(u5)
t = a ->b, b = e ->d,   a = e->d,     c= e
(u5)
t = (e->d) ->b,     b = e ->d,   a = e->d,     c= e
(u5)
t = (e->d) ->e ->d,     b = e ->d,   a = e->d,     c= e

-}



{-
row::[[Int]] -> Int -> [Int]
row m x = m !! (x-1)

col::[[Int]] -> Int -> [Int]
col m k = [x !! (k-1) | x<-m]

cols::[[Int]] -> [[Int]]
cols m = [col m x | x<- [1..(length m)]]

mult::[[Int]]->[[Int]]->[[Int]]
mult ma mb = [map (multrc (row ma x)) (cols mb) | x <- [1..length ma]]
multrc r c = sum [x*y | (x,y) <- zip r c]

-}
{-
row::[[Int]] -> Int -> [Int]
row m r = m !! (r-1)

col::[[Int]] -> Int -> [Int]
col m k = [x !! (k-1) | x<-m]

cols::[[Int]] -> [[Int]]
cols m = [col m k | k <- [1 .. length m]]

--mult::[[Int]]->[[Int]]->[[Int]] 
--mult m n = c
mult ma mb = [map (multrc (row ma x)) (cols mb) | x <- [1..length ma]]
multrc r c = sum [x*y | (x,y) <- zip r c]
-}
-------3-------
--E ::= Pos | E E * | E E + | E E –
--Pos ::= Digit | DigitPos
--Digit ::= 0 | 1 | ... 8 | 9

--eval::IO()
--eval = evalH []

-}