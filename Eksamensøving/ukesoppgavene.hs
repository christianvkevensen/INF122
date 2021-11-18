---uke1----
dobb :: [Int] -> [Int]
dobb xs = map (2*) xs

fire :: [Int] -> [Int]
fire xs = map (4*) xs

fire2 :: [Int] -> [Int]
fire2 [] = []
fire2 (x:xs) = (x*4):fire2 xs

flett :: [Int] -> [Int] -> [Int]
flett [] ys = ys
flett xs [] = xs
flett (x:xs) (y:ys) = if (x >= y) then y:flett (x:xs) ys else x:flett xs (y:ys) 

ele :: Int -> [a] -> a
ele n xs = head((drop (n-1) xs))
  
addobb :: [Int] -> [Int]
addobb [] = []
addobb xs = xs ++ (dobb xs)

pali xs = xs == (reverse xs)

---Uke3----
f1::a->(a,a)
f1 = \x -> (x,x)

f :: Int -> Int -> Int
f = \x -> \y -> x
g :: (Int, Int) -> Int
g (x,y) = x

-----Uke4-----
isPrefix :: Eq t => [t] -> [t] -> Bool
isPrefix [] _ = True
isPrefix (x:xs) (y:ys)  | x==y = isPrefix xs ys
                        | otherwise = False

rem1 :: Eq a => [a] -> a -> [a]
rem1 (x:xs) y = if x==y then xs else x:rem1 xs y

-----Uke6-----
remg :: [a] -> (a -> Bool) -> [a]
remg [] pr = []
remg (x:xs) pr = if (pr x) then xs else x:remg xs pr

------Uke7----
data Ast = V Int | P Ast Ast | M Ast Ast
eval :: Ast -> Int
eval (V x) = x
eval (P a1 a2) = (eval a1) + (eval a2)
eval (M a1 a2) = (eval a1) * (eval a2)

evalb :: Ast -> Bool
evalb (V x) = if mod x 2 == 0 then False else True 
evalb (P a1 a2) = evalb a1 || evalb a2
evalb (M a1 a2) = evalb a1 && evalb a2

-------Uke8------
trekanthelper x = reverse [concat(replicate (x-n) "* ") | n <- [1..x]]
juletrehelper x = reverse [concat((replicate n " ")++ (replicate (x-n) "* ")) | n <- [1..x]]
trekanter x = reverse [concat((replicate n " ")++ (replicate (x-n) "* ") ++ (replicate n " ")) | n <- [1..x]]

replicator tree n m = [replicate (m*2) ' '  | _ <- [1..n] ] ++ tree



trekant :: Int -> IO ()
trekant b = do
    mapM_ putStrLn (trekanthelper b)

juletre:: Int -> IO() 
juletre b = do
    mapM_ putStrLn (juletrehelper b)

trekanterBom x y z = do
    let maxT = max x (max y z)

        trekant1 = replicator (trekanter x) (maxT - x) x 
        trekant2 = replicator (trekanter y) (maxT - y) y 
        trekant3 = replicator (trekanter z) (maxT - z) z 
        
        trekantListe = [(trekant1 !! d) ++(trekant2 !! d) ++(trekant3 !! d) | d <- [0..(maxT-1)]]

    mapM_ putStrLn(trekantListe)



data FileOrFolder = File Int | Folder [ FileOrFolder ]

prettyPrint :: FileOrFolder -> IO () 
prettyPrint f = prettyPrint2 f 0

prettyPrint2 (File x) n= do putStrLn ((replicate (4*n) ' ' ) ++ "-File " ++ show x) 
prettyPrint2 (Folder list) n= do 
    putStrLn((replicate (4*n) ' ') ++ "-Folder " ++ show (length list))  
    mapM_ (\x -> prettyPrint2 x (n+1)) list



-------Uke9------
gRep::(t->Bool)->t->[t]->[t]
gRep _ _ [] = []
gRep pr y (x:xs) = if (pr x) then y:gRep pr y xs else x:gRep pr y xs

gRepM::(t->Bool)->t->[t]->[t]
gRepM pr y xs = map(\x -> if (pr x) then y else x) xs

