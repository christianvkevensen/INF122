----1-----
{-
foldl (-) 0 [2,4,6]
-2, -6, -12 = -12

foldr (-) 0 [2,4,6]
6-0= 6 
4 -6 = -2
2- (-2) = 4
 
-}

-----2-----
funk :: Eq a => [a] -> Bool
funk liste = funkHelp(funkP liste)
  
funkHelp [] = True
funkHelp (x:xs) = if elem x xs then False else funkHelp xs

funkP liste = [(liste !! x)| x<-[0..((length liste) - 1)], even x]

avb::Eq a => [a]->[a]->[a]
avb str ls = if (funk ls) 
    then if (odd (length ls)) 
        then filter (\x-> not(x==(last ls))) str
        else str
    else str
