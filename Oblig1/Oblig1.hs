-- TODO: Christian Evensen
module Oblig1 where

dictionary = [
        ("bb",["Big Brother"]),
        ("dep",["department"]),
        ("sec", ["Sector"]),
        ("doubleplusgood",["excellent", "fabulous", "fantastic", "best"]),
        ("doubleplusungood", ["terrible", "horrible", "worst"]),
        ("Ingsoc", ["English Socialism"]),
        ("joycamp", ["labour camp"]),
        ("Oldspeak", ["Standard English", "English"]),
        ("oldthink", ["objectivity", "rationalism", "democracy"]),
        ("thinkpol", ["The Thought Police"]),
        ("prolefeed", ["Popular culture", "pop-culture"]),
        ("crimethink", ["liberty", "equality", "privacy", "thoughtcrime"]),
        ("fullwise", ["fully", "completely", "totally"]),
        ("goodthink", ["political orthodoxy", "politically orthodox thought", "orthodox thought"]),
        ("goodwise", ["well"]),
        ("ownlife", ["anti-social tendency", "solitude", "individualism"]),
        ("plusgood", ["very good", "great"]),
        ("plusungood", ["very bad"]),
        ("misprint", ["error", "misprediction"]),
        ("Miniluv", ["The Ministry of Love"]),
        ("Minipax", ["The Ministry of Peace"]),
        ("Minitrue", ["The Ministry of Truth"]),
        ("Miniplenty", ["The Ministry of Plenty"]),
        ("bellyfeel", ["blind, enthusiastic acceptance"]),
        ("doublethink", ["believing two contradictory ideas"]),
        ("duckspeak", ["vocal support of political orthodoxies"]),
        ("un", ["not"]),
        ("peace", ["war"]),
        ("strength", ["ignorance"]),
        -- The next line contains a list of forbidden words that don't have a translation to Newspeak, these should be replaced with '*'s
        ("",["freedom", "revolution", "fun", "diary", "surveillance", "Great Britain", "Winston Smith", "Julia"])
        ]


-- Oppgave 1 ----------------------------------------------------
isPrefix :: String -> String -> Bool 
isPrefix [] _ = True
isPrefix _ [] = False
isPrefix (x:xs) (y:ys)  | x /= y = False
                        | otherwise = isPrefix xs ys 


-- Oppgave 2 ----------------------------------------------------
locateHelp :: String -> [Char] -> Int -> [(Int, Int)]
locateHelp _ [] _=[]
locateHelp x (y:ys) a   | isPrefix x (y:ys) == False = locateHelp x ys (a+1)
                        | otherwise = (a, length x + a): locateHelp x ys (a+1)

locate :: String -> String -> [(Int,Int)]
locate [] [] = []
locate [] _ = []
locate x y = locateHelp x y 0       

-- Oppgave 3 -----------------------:-----------------------------
translateLook :: Eq t => t -> [([Char], [t])] -> [Char]
translateLook a (((b,(c)):ds))  | length([x | x<-c, a==x]) > 0 = b
                                | null (ds) = ""
                                | otherwise = translateLook a ds
                                
translate :: String -> String
translate [] = []
translate a = translateLook a dictionary

-- Oppgave 4 ----------------------------------------------------
subStr (a,b) d  | a==0 = take b d
                | otherwise = take (b-a) (drop a d) 

findWords [] = []
findWords (((b,(c)):ds)) = [x | x <- c]:findWords ds

--lager en liste og concat-er for alle treff som kan gi translate (og uten translate f.eks freedom)
concatList = concat (findWords dictionary)

-- siden x < y alltid, og indeksene i de forskjellige tuplene ikke vil overlappes, så lar jeg vær å legge inn caser for det. 
replaceHelp _ [] = []
replaceHelp (x,y) a     | translate(subStr(x,y) a) == "" = replicate (y-x) ('*')
                        | elem (subStr(x,y) a) concatList = translate(subStr(x,y) a)
                        | otherwise = subStr (x,y) a

inserter (x,y) a = take x a ++ replaceHelp (x,y) a ++ drop y a

revreplace [] a = []
revreplace ((x,y):xs) a = if(null xs) then inserter (x,y) a else revreplace xs (inserter (x,y) a)

replace :: [(Int,Int)] -> String -> String 
replace [] a = a
replace x a= revreplace (reverse (qs x)) a 

-- Oppgave 5 ----------------------------------------------------
toNewSpeakHelper a [] = []
toNewSpeakHelper a (x:xs)       | locate x a == [] = toNewSpeakHelper a xs
                                | otherwise = locate x a : toNewSpeakHelper a xs

toNewspeak :: String -> String 
toNewspeak [] = []
toNewspeak a = replace (concat(toNewSpeakHelper a concatList)) a


-- Oppgave 6 ----------------------------------------------------
countLetter [] b = 0
countLetter a b = length $ filter (== b) a 

countLetters a [] = 0
countLetters a (b:bs) = (length $ filter (== b) a) + (countLetters a bs)

-- strippe bort antall bokstaver, sitter igjen med bokstavene som ikke kom med videre (uten duplikater)
stripAway b c= [a | a<-b, not (a `elem` c)]

--lager en liste med duplikater som ikke eksisterer i listen stripAway, 
--der hvor differansen av antall bokstaver funnet i original string minus ny string >= 1
stripAwayTest b c= [a | a<-b, (countLetter b a) - (countLetter c a) >= 1 && not (a `elem` stripAway b c)]

removeDup [] = []
removeDup (b:bs)        | b `elem` bs = removeDup bs
                        | otherwise = b : removeDup bs

--fjerner duplikater av listen fra stripAwayTest
stripAwayW a b= removeDup (stripAwayTest a b)

--returnerer differansen mellom bokstav(er) fra gammel til ny liste av
addDup a b c= countLetters a c - countLetters b c 

lengthStrip a b= length (stripAway a b) + addDup a b(stripAwayW a b)

percent [] [] = 0
percent _ [] = 100
percent a b = round(100*((fromIntegral(lengthStrip a b)) / fromIntegral(length a) ))

analytics :: String -> String -> Int 
analytics a b= percent a b


-- Oppgave 7 ----------------------------------------------------
main :: String -> (String, Int)
main [] = ("", 0)
main a = (toNewspeak a, analytics a (toNewspeak a))

--qs
qs [] = []
qs (x:xs) = qs[y|y<-xs,y<=x]++[x]++qs[y|y<-xs,y>x]