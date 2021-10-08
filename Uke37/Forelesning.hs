twice = \f -> f . f

li1 xs = [x+1 | x <- xs]
lisq xs = [x*x | x<-xs]

rad n matrise = matrise !! (n-1)

kol k matrise = map (rad k) matrise
