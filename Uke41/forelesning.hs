import System.IO
import Data.Char
--f:: TypeA -> IO TypeB
-- kalles kasjjoner derfsom de har IO i sin type
--putChar
--putStr
skriv x = putChar x
say ss= putStrLn ss

navn = putStr "Hello"

navn2 = "Hei"

hei = do
    putStr "Hei"
    putStr "Går det bra"
    putStr "?"

ak = do
    res <- getLine
    putStrLn("Velkommen " ++ res)
--aksjon :: IO TypeB
--res :: TypeB

--funksjoner kan ikke kalle aksjoner, aksjoner kan fritt kalle funksjoner

mm = do 
    line <- getLine
    if(null line) then return ()
    else do
       putStr (unwords (map reverse (words line))) 
       mm

main = do
    putStrLn "Ditt fornavn?"
    fornavn <- getLine
    putStrLn "Ditt etternavn?"
    etternavn <- getLine
    let stortFor = map toUpper fornavn
        stortEtt = map toUpper etternavn
    putStr $ "Hei, " ++stortFor++ " " ++ stortEtt++ ". Alt ok?"

--getLine = do 
--            x <- getChar
--            if x =='\n' then return []
--            else
--                do xs <- getLine
--                        return (x:xs)

uno = do
    putStr "Hvor mange tall?"
    ant <- getLine
    --read ant :: Int
    res <- aux (read ant :: Int) 0
    putStrLn ("Summen er " ++ show res)

aux 0 x= return x
aux n x = do 
        tall <- getLine
        aux (n-1) (x + (read tall :: Int))

tall x = (read x :: Int)

duo = aux1 0

aux1 z = do
    x <- getLine
    if (tall x == 0) then return x
        else aux1 (z + tall x)

tre = do 
        putStr ("Skriv tallene i en linje: ")
        xs <- getLine
        putStrLn ("Summen er: " ++ show (sum [tall ts | ts <- words xs]))

clr = putStr "\ESC[2J"
goto :: (Int, Int) -> IO ()
goto(x,y) = putStr("\ESC["++show y ++";"++show x ++"H")

writeAt (x,y) str = do 
                    goto (x,y)
                    putStr str

brett nr = do 
            clr
            writeTop nr
            putStrLn("")
            writeRows 1 nr

writeTop n = writeAt (3,0)  (concat [show (mod i 10) ++ " " |i<- [1..n]])

writeRows a nr = if a>nr then return ()
                else do writeRow a nr
                        writeRows (a+1) nr

writeRow i nr = putStrLn (show (mod i 10) ++ " " ++ concat((take nr (repeat ". "))))

vis n = [print i | i <- [1..n]]
visS n = sequence_ [ print i | i<-[1..n]]
de n = sequence_ [ return () | i<-[1..n]]