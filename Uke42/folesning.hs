import Data.Char
import System.IO
import System.Directory

flyttLn fn = do
    content <- readFile fn
    let (l1:rest) = lines content 
        newContent = unlines (rest++[l1])
    writeFile fn newContent

fto filNavn = do
        innhold <- readFile filNavn
        let res = map toUpper innhold 
        if(length innhold > 0) then writeFile filNavn res
        else return ()

flyttLn3 fn = do 
            fh <- openFile fn ReadMode
            content <- hGetContents fh
            let (l1:rest) = lines content
                newContent = unlines (rest++[l1])
            (fn2, fh2) <- openTempFile "." "temp"
            hPutStr fh2 newContent
            hClose fh
            hClose fh2
            removeFile fn
            renameFile fn2 fn

main = inter []
inter liste = do
    putStr "n x y / f / v / t / s filnavn / a filnavn / l filnavn / q: "
    ordre <- getLine
    let ord = words ordre
        cmd = head ord
    if (cmd == "n") then
        inter ((head (tail ord) , last ord) : liste)
    else if (cmd == "f") then if (null liste) then inter (tail liste) else inter (tail liste)
    else if (cmd == "v") then do
        putStrLn (vis liste)
        inter liste
    else if (cmd == "t") then inter []
    else if (cmd == "s") then do
        writeFile (last ord) (vis liste)
        inter []
    else if (cmd == "a") then do
        -- appendFil (last ord) (vis liste)
        app (last ord) (vis liste)
        inter liste
    else if (cmd == "l") then do 
        ny <- readFile (last ord)
        inter (reverse(pairs (words ny)))
    else if (cmd == "q") then return ()
    else do putStrLn ("Ugyldig kommando")
            inter liste
vis = concat . map (\x -> (fst x ++ ":" ++ snd x ++ " "))

pairs [] = []
pairs (x:xs) = (takeWhile (/= ':') x, tail(dropWhile(/= ':') x)) : pairs xs

app fn txt= do 
            fh <- openFile fn ReadMode
            content <- hGetContents fh
            let nytt = content ++ txt
            (fn2, fh2) <- openTempFile "." "temp"
            hPutStr fh2 nytt
            hClose fh
            hClose fh2
            removeFile fn
            renameFile fn2 fn