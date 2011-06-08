--1
myLast (x:[]) = x
myLast (x:xs) = myLast xs

--2
myButLast (x:y:[]) = x
myButLast (x:xs) = myButLast xs

--3
myElementAt (x:xs) 1 = x
myElementAt (x:xs) y |y >1 = myElementAt xs (y-1)
                     |y<=0 = x

--4
myLength [] = 0
myLength (x:xs) = myLength (xs) + 1

--5
myReverse [] = []
myReverse (x:xs) = (myReverse xs) ++ [x]

--6
isPalindrome [] = True
isPalindrome (x:[]) = True
isPalindrome (x:xs) = (x == last(xs)) && isPalindrome (init xs)


--7
myFlatten []  = []        
myFlatten (x:xs) = (myFlatten x) ++ (myFlatten xs)
myFlatten x  = [x]

--8
myCompress [] = []
myCompress (x:[]) = x:[]
myCompress (x:y:xs) | x==y = myCompress (x:xs)
                    | otherwise = x:myCompress(y:xs)

--9
myPack' [] = []
myPack' (x:xs) = [x]: myPack' (xs)
myPack'' [[]] = [[]]
myPack'' ((x:xs):[]) = (x:xs):[]
myPack'' ((x:xs):(y:ys):zs) | x==y = [((x:xs)++(y:ys))] ++ (myPack'' zs)
                            | otherwise = [(x:xs),(y:ys)]++ (myPack'' zs)


