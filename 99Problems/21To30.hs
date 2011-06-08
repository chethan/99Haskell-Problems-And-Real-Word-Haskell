myInsertAt [] a _ = [a]
myInsertAt (x:xs) a 1 = a:x:xs
myInsertAt (x:xs) a b = x:(myInsertAt xs a (b-1))

myRange a b  =  let temp a b xs | a > b = xs
                                | otherwise = temp (a+1) b (xs++[a])
                in temp a b []

myCombination n xs = [(a,b,c)| a <- xs , b <- xs , c <- xs]                