import Data.Char
eval::IO()
eval = do
    utregn [] ""

utregn liste msg = do
    putStrLn msg
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
            utregn (r:nyL) ""
        else utregn liste "feil"