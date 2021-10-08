import System.IO
--A
trekant :: Int -> IO ()
trekant a = putStr (unlines (trekanthlp a)) 

trekanthlp a = [unwords(replicate x "*") | x <- [1..a]]

--B
trekant2 :: Int -> IO ()
trekant2 a = putStr (unlines (trekanthlp2 a)) 

trekanthlp2 a = [replicate (a-x) ' ' ++ unwords(replicate x "*") ++ replicate (a-x+1) ' ' | x <- [1..a]] --legge til replicate bak og

--C
--trekanter :: Int -> Int -> Int -> IO ()
--trekanter x y z = putStr (unlines (stringA x y z (maks x y z)))
--trekanter x y z = putStr (unlines (trekanterNy x y z))

--stringA x y z d = if( d /= 0) 
--    then [replicate d ' ' ++ unwords(replicate (x-(d-1)) "*")  ++ replicate d ' ' ++ replicate d ' ' ++ unwords(replicate (y-(d-1)) "*")  ++ replicate d ' ' ++ replicate d ' ' ++ unwords(replicate (z-(d-1)) "*") ++ replicate d ' '] ++ stringA x y z (d-1)
--    else []
    
replicater x n t= replicate x (replicate (n*2) ' ') ++ t

trekanter :: Int -> Int -> Int -> IO ()
trekanter x y z = do
        let maks = max x (max y z)

        let xN = replicater (maks - x) x (trekanthlp2 x)
        let yN = replicater (maks - y) y (trekanthlp2 y)
        let zN = replicater (maks - z) z (trekanthlp2 z)

        let listeT = [(xN !! a) ++ (yN !! a) ++ (zN !! a) | a <- [0 .. (maks-1)]]
        
        mapM putStrLn listeT

        return ()

{-        
stringA x y z d     | x >= y && x >= z && y>=z  && d /= 0 = [replicate d ' ' ++ unwords(replicate (x-(d-1)) "*")  ++ replicate d ' ' ++ replicate d ' ' ++ unwords(replicate (y-(d-1)) "*")  ++ replicate d ' ' ++ replicate d ' ' ++ unwords(replicate (z-(d-1)) "*") ++ replicate d ' '] ++ stringA x y z (d-1)
                    | d /= 0 = [replicate ( x + x + 1 ) ' '] 
                    | otherwise = []


maks x y z = max x (max y z)

-}
--D
data FileOrFolder = File Int | Folder [ FileOrFolder ]
prettyPrint :: FileOrFolder -> IO () 
prettyPrint (File x) = return ()
prettyPrint (Folder x) = putStrLn $ prettyPrintHlp (Folder x) 0

prettyPrintHlp (File x) c=  (take (c*3)(repeat ' ')) ++ "-File " ++ show x ++ "\n"
prettyPrintHlp (Folder x) c= concat $ ((take (c*3) (repeat ' ')) ++ "-Folder " ++ show (length x) ++ "\n") : [ prettyPrintHlp y (c+1) | y <-x]
