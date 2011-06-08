myDup [] = []
myDup (x:xs) = x:x:myDup xs
myReplicate [] num = []
myReplicate (x:xs) num = (rep' x num)++(myReplicate (xs) num)
                         where
                          rep' x 0 = []
                          rep' x 1 = [x]
                          rep' x num = [x] ++ (rep' x (num-1))

myDrop [] _ = []
myDrop (x:xs) n = myDrop' (x:xs) n n
                  where
                    myDrop' [] _ _ = []
                    myDrop' (x:xs) n 1 = myDrop' (xs) n n
                    myDrop' (x:xs) n m = x : ( myDrop' (xs) n (m-1))

mySplit xs 0 = ([], xs)
mySplit (x:xs) n = let (f,l) = mySplit xs (n-1) in (x : f, l)

mySlice [] _ _  = []
mySlice (x:xs) n m | n > m = []
                   | m == 1 = []
                   | n == 1 = x: (mySlice (xs) 1 (m-1))
                   | otherwise = mySlice (xs) (n-1) (m-1)

myRotate [] _ = []
myRotate l@(x:xs) n | n == 0 = (x:xs)
                  | n > 0 = myRotate (xs ++ [x]) (n-1)
                  | n < 0 = myRotate ([last l]++(init l)) (n+1)

myRemove [] _ = ([],[])
myRemove (x:xs) 1 = ([x],xs)
myRemove (x:xs) n = let (a,b) = myRemove xs (n-1) in (a, x:b)