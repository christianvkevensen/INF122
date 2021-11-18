row::[[Int]] -> Int -> [Int]
row m r = m !! (r-1)

col::[[Int]] -> Int -> [Int]
col m k = [x !! (k-1) |x<-m]

cols::[[Int]] -> [[Int]]
cols m1 = [col m1 x| x<-[1..length m1]]

--mult::[[Int]]->[[Int]]->[[Int]]
--mult m1 m2 = multHelp m1 (cols m2) 

--multHelp m1 m2 = [x*y |xs<-m1, ys<-m2, x <-xs, y<-ys]
mult::[[Int]]->[[Int]]->[[Int]]
mult m1 m2 = [[hlp (head m1) (head m2), hlp (head m1) (last m2)],[hlp (last m1) (head m2), hlp (last m1) (last m2)]]
hlp m1 m2 = (head m1 * head m2) + (last m1 *last m2)
hlp2 m1 m2 = sum[x*y |(x,y) <- zip m1 m2]


--[[1,2],[3,4]] [[3,5],[4,6]]