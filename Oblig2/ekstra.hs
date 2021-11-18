{-
hanoi 0 _ _ _          = []
hanoi 1 start end _    = [(start, end)]
hanoi n start end temp =
    let nMinusOne = subtract 1 n
    in hanoi nMinusOne start temp end ++
       hanoi 1 start end temp ++
       hanoi nMinusOne temp end start   
-}
----hint----
--hint trekantList n = listeHintSjekk trekantList n 
--listeHintSjekk trekantList n = undefined

--listeHint trekantList n     | head trekantList == n = listeHint (init trekantList) (n-1)


-- [[[1,2,3,4,5],[],[]], [[2,3,4,5],[1],[1]], [[1,2,3,4,5],[3],[]]]


{-
 then do
                let sf = read(head liste)::Int
                let st = read(last liste)::Int
                if (sf > 0 && sf < 4 && st > 0 && st < 4) 
                    then do
                        let f = head(trekantList !! (sf-1))
                        let fL = tail(trekantList !! (sf-1))
                        let tL = f : trekantList !! (st-1)
                        let ubrukt = finnUbrukt (read(head liste)::Int) (read(last liste)::Int)
                        let ubruktL = trekantList !! ubrukt
                        if (sf == st) 
                            then spillTrekant n trekantList tidligereList moves "The numbers cant be the same"
                        else if (length(trekantList !! (sf-1)) == 0)
                            then spillTrekant n trekantList tidligereList moves "Empty tower"
                                    else do
                                        if (length (trekantList !! (st-1)) == 0)
                                            then do
                                                let nyL = [if ((sf-1) == index) then fL else if((st-1) == index) then tL else ubruktL | index <- [0..2]]
                                                spillTrekant n nyL (trekantList:tidligereList) (moves+1) ""
                                        else if (head (trekantList !! (st-1)) > f) 
                                            then do
                                                let nyL = [if ((sf-1) == index) then fL else if((st-1) == index) then tL else ubruktL | index <- [0..2]]
                                                spillTrekant n nyL (trekantList:tidligereList) (moves+1) ""
                                        else 
                                            spillTrekant n trekantList tidligereList moves  "Cant move this ring to other ring"
                         else spillTrekant (read(last(liste))::Int) (trekantListe (read(last(liste))::Int)) tidligereList moves  "Wrong input, the number must be between 1-3S"
-}