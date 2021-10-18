import System.IO
import Data.Char

--A
--lagde først denne med map, ble ikke så fornøyd så prøvde på en annen måte.
gRep::(t->Bool)->t->[t]->[t]
gRep a b [] = []
gRep pr y (x:xs) = if head(map pr (x:[])) then y : gRep pr y xs else x : gRep pr y xs

gRep1::(t->Bool)->t->[t]->[t]
gRep1 pr y = map(\x -> if pr x then y else x)

gRep2::(t->Bool)->t->[t]->[t]
gRep2 a b [] = []
gRep2 pr y (x:xs) = if pr x == True then y : gRep2 pr y xs else x : gRep2 pr y xs
                    
--B
type Board = [Int]

putBoard :: Board -> IO ()
putBoard board = do 
    clr
    writeRows2 board 1

writeRows2 [] a = return ()
writeRows2 (b:board) a = do
    writeRow b a 
    writeRows2 board (a+1)


writeRow nr i = putStrLn (show (mod i 10) ++ ": " ++ concat((take nr (repeat "* "))))

clr = putStr "\ESC[2J"

--C

next :: Int -> Int
next 1 = 2
next 2 = 1


initial :: Board
initial = [5,4,3,2,1]

finished :: Board -> Bool
finished = all (== 0)

valid :: Board -> Int -> Int -> Bool
valid board row num = board !! (row-1) >= num

move :: Board -> Int -> Int -> Board
move board row num = [update r n | (r,n) <- zip [1..] board]
    where update r n = if r == row then n-num else n


getDigit :: String -> IO Int
getDigit prompt = do 
    putStr prompt
    s <- getLine
    newline
    if not (null s) && all isDigit s then return (read s)
    else do 
        putStrLn "ERROR: Invalid row"
        getDigit prompt

newline :: IO ()
newline = putChar '\n'

getList prompt = do 
    putStr prompt
    input <- getLine
    let liste = concat(words(input))
    newline
    if (not (input == liste)) && ((length liste) == 2) && (isDigit(head(liste))) && (isDigit(last(liste))) then return [digitToInt x | x <- liste]
    else do 
        putStrLn "ERROR: Invalid row"
        getList prompt

play :: Board -> Int -> IO ()
play board player = do 
    newline
    putBoard board
    if finished board then do 
        newline
        putStr "Player "
        putStr (show (next player))
        putStrLn " wins!!"
    else do 
        newline
        putStr "Player "
        putStrLn (show player)
        row <- getDigit "Enter a row number: "
        num <- getDigit "Stars to remove : "
        if valid board row num then play (move board row num) (next player)
        else do 
            newline
            putStrLn "ERROR: Invalid move"
            play board player
        

play2 :: Board -> Int -> IO ()
play2 board player = do 
    newline
    putBoard board
    if finished board then do 
        newline
        putStr "Player "
        putStr (show (next player))
        putStrLn " wins!!"
    else do 
        newline
        putStr "Player "
        putStrLn (show player)
        liste <- getList "Enter a row number and star number: "
        if valid board (head liste) (last liste) then play2 (move board (head liste) (last liste)) (next player)
        else do
            newline
            putStrLn "ERROR: Invalid move"
            play2 board player

nim2 :: IO ()
nim2 = play2 initial 1

--d
play3 :: Board -> Int -> [Board] -> IO ()
play3 board player listeBoard= do 
    newline
    putBoard board
    if finished board then do 
        newline
        putStr "Player "
        putStr (show (next player))
        putStrLn " wins!!"
    else do 
        newline
        putStr "Player "
        putStrLn (show player)
        liste <- getList "Enter a row number and star number: "
        if ((head liste) == 0 && not (null listeBoard))
            then play3 (head listeBoard) (next player) (tail listeBoard)
            else 
                if ((head liste) == 0 && (null listeBoard))
                    then do
                        play3 board player listeBoard
                        else
                            if valid board (head liste) (last liste) 
                                then play3 (move board (head liste) (last liste)) (next player) (board : listeBoard)
                            else do
                                newline
                                putStrLn "ERROR: Invalid move"
                                play3 board player listeBoard

nim3 :: IO ()
nim3 = play3 initial 1 []