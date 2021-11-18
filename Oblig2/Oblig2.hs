import System.IO()
import Data.Char

--Christian Evensen

nylinje :: IO ()
nylinje = putStr"\n"

clr :: IO ()
clr = putStr "\ESC[2J"

--oppretter visuell representasjon av ringene med mellomrom i liste
trekant :: Int -> [Int] -> [[Char]]
trekant maks liste = [replicate (maks-x) ' ' ++ unwords(replicate x "#") ++ replicate (maks-x+1) ' ' | x <- liste]

replikator :: Int -> Int -> [[Char]] -> [[Char]]
replikator x n t= replicate x (replicate n ' ' ++ "|" ++ replicate (n+1) ' ') ++ t 

--oppretter liste med start
trekantListe :: (Num a, Enum a) => a -> [[a]]
trekantListe n = [[1..n], [], []]

--lager brettet
trekanter :: Int -> [[Int]] -> IO ()
trekanter maksring liste = do
        let maks =maksring + 1

            xN = replikator (maks - length(liste !! 0)) maksring (trekant maks (liste !! 0))
            yN = replikator (maks - length(liste !! 1)) maksring (trekant maks (liste !! 1))
            zN = replikator (maks - length(liste !! 2)) maksring (trekant maks (liste !! 2))

            listeT = [(xN !! a) ++ (yN !! a) ++ (zN !! a) | a <- [0 .. (maks-1)]]
        
        mapM_ putStrLn listeT

        return ()

--henter startsinput til spiller, og sjekker om den er gyldig før den sender videre en string til main
hentStart :: IO [Char]
hentStart = do
    input <- getLine
    let liste = words input
    if(length liste > 2) then do
        putStrLn "ERROR: invalid input. Start a new game with: b <nbOfRings>, or quit with : q" 
        hentStart
        else
        if (length liste == 1 && head liste=="q") then return "f"
        else
            if((length liste == 2 && sjekkTall (last(liste))) 
                && head liste == "b" && (12>(read(last liste)::Integer)) && (read(last liste)::Integer) > 0)
                then do return (last liste)
                    else do
                    putStrLn "ERROR: invalid input. Start a new game with: b <nbOfRings> (1-11), or quit with : q"
                    hentStart

main :: IO()
main = do
    putStrLn("Start a new game with: b <nbOfRings>, or quit with : q")
    kmd <- hentStart
    if(kmd == "f") then return()
    else do
        spillSpill (read(kmd)::Int) (trekantListe (read(kmd)::Int)) [] 0 ""

--selve spillet, meldingene er på engelsk siden det stod slik i oppgaven. 
-- n er største ring, trekantList er nåværende brett, tidligereList er alle tidligere brett, 
--trekk er antall trekk og melding er en evt feilmelding eller annen relevant melding
spillSpill :: Int -> [[Int]] -> [[[Int]]] -> Int -> [Char] -> IO ()        
spillSpill n trekantList tidligereList trekk melding= do
        clr
        nylinje
        trekanter n trekantList
        --sjekk hvis spiller har vunnet
        if(length (trekantList !! 2) == n)
        then do
            putStrLn("Congratulations! You have won this game! At level: " ++ show n ++ " with " ++ show trekk ++ " amounts of moves. ")
            main
        else do
        putStr("Number of moves: ")
        putStrLn(show trekk)
        putStrLn(melding)
        input <- getLine
        let liste = words(input)
        --if null input
        if(null liste) then do
            spillSpill n trekantList tidligereList trekk "Wrong input, commands: q, b n, z n, h, f t"
        --slutt
        else if(input == "q") then return()
        --start på nytt
        else if (head liste == "b" && sjekkTall (last(liste))) 
            then if((read(last(liste))::Int)>0&&((read(last(liste))::Int) < 12)) 
                then spillSpill (read(last(liste))::Int) (trekantListe (read(last(liste))::Int)) [] 0 "New game"
                else spillSpill n trekantList tidligereList trekk "The number was too high or 0, max is 11 commands: q, b n, z n, h, f t"
        --gå tilbake n steg
        else if(head liste == "z" && length liste == 2 && sjekkTall (last(liste))) 
            then if (length tidligereList > 0 && (read(last liste)::Int)> 0)
                then do
                    let trekantListeNy = tilbake (read(last liste)::Int) tidligereList
                        nyTrekk = fjernTrekk trekk (read(last liste)::Int) 
                        nytidligereList = tidligereListeNy (read(last liste)::Int) tidligereList

                    spillSpill n trekantListeNy nytidligereList nyTrekk "" 
            else spillSpill n trekantList tidligereList trekk "Make a move first/0 is not allowed, commands: q, b n, z n, h, f t"
        --hint
        else if (head liste == "h") then spillSpill n trekantList tidligereList trekk ("Hint: "++show (hint trekantList n [])) 
        
        --flytte
        else if(sjekkTall (head(liste))&& sjekkTall (last(liste)) && length liste < 3) 
            then do
                let sf = read(head liste)::Int
                    st = read(last liste)::Int
                    
                if (sf > 0 && sf < 4 && st > 0 && st < 4) 
                    then do
                        if (sf == st) 
                            then spillSpill n trekantList tidligereList trekk "The numbers cant be the same, commands: q, b n, z n, h, f t"
                        else if (length(trekantList !! (sf-1)) == 0)
                            then spillSpill n trekantList tidligereList trekk "Empty tower, commands: q, b n, z n, h, ft"
                                    else do
                                        let f = head(trekantList !! (sf-1))
                                            t = head (trekantList !! (st-1))
                                        if ((length (trekantList !! (st-1)) == 0) || t > f)
                                            then do
                                                let nyL = flyttRing sf st trekantList
                                                spillSpill n nyL (trekantList:tidligereList) (trekk+1) ""
                                        else 
                                            spillSpill n trekantList tidligereList trekk  "Cant move this ring to other ring, commands: q, b n, z n, h, f t"
                         else spillSpill n trekantList tidligereList trekk  "Wrong input, the number must be between 1-3S, commands: q, b n, z n, h, f t"
                    else 
                        spillSpill n trekantList tidligereList trekk  "Wrong input, commands: q, b n, z n, h, f t"

--sjekker hvis første/siste del av input er et tall
sjekkTall :: [Char] -> Bool
sjekkTall x = and[isDigit k | k<-x]

--finner den urørte listen sin indeks
finnUbrukt :: (Num a, Enum a, Eq a) => a -> a -> a
finnUbrukt sf st = head([x | x<-[0..2], x/=(sf-1) && x/=(st-1)])

--z n funksjon, går tilbake n trekk
tilbake :: Int -> [a] -> a
tilbake n tidligereList | n>length tidligereList = last tidligereList
                        | otherwise = tidligereList !! (n-1)

--fikser ny liste, brukes etter tilbakefunksjonen er fullført                                
tidligereListeNy :: Int -> [a] -> [a]
tidligereListeNy n tidligereListe   | n<length tidligereListe = drop n tidligereListe
                                    | otherwise = []                        

--trekker antall moves utført, i takt med z n
fjernTrekk :: (Ord p, Num p) => p -> p -> p
fjernTrekk trekk n = if(n>trekk) then 0 else (trekk - n)

--flytter ring stolpe fra til stolpe til og setter sammen i riktig rekkefølge
flyttRing :: Int -> Int -> [[a]] -> [[a]]
flyttRing sf st trekantList = [if ((st-1) == indeks) then tL else if((sf-1) == indeks) then fL else ubruktL | indeks <- [0..2]]
    where
        fL = tail(trekantList !! (sf-1))
        tL = head(trekantList !! (sf-1)):(trekantList !! (st-1))
        ubruktL = (trekantList !! (finnUbrukt sf st))

--henter første hint trekk, som er siste i listen hint2 returnerer (hint2 returnerer en tuppel, listen ligger i snd)
hint :: (Eq a, Num a) => [[a]] -> a -> [(Int, Int)] -> (Int, Int)
hint trekantList n liste = last(snd (hint2 trekantList n liste 3))
   
--hint funksjon som oppretter både listen med optimale trekk og trekantListen
hint2 ::(Eq a, Num a) => [[a]] -> a -> [(Int, Int)] -> Int -> ([[a]], [(Int, Int)])
hint2 trekantList n liste t= do
    if n == 0 then do (trekantList, liste)
    else if elem n (trekantList !! (t-1)) 
        then 
            hint2 trekantList (n-1) liste t
        else
            let (nyTrekantList, nyTrekk) = hint2 trekantList (n-1) liste h in
                hint2 (flyttRing s t nyTrekantList) (n-1) ((s,t):nyTrekk) t
                    where 
                        s = (head([k| k<- [0..2], elem n (trekantList !! k)]) +1) 
                        h = (finnUbrukt s t) + 1
