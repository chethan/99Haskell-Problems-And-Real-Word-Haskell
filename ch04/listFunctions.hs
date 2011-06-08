myReverse :: [a] -> [a]
myReverse [] = []
myReverse (x:xs) = (myReverse xs) ++ [x]

myAll :: (a->Bool) -> [a] -> Bool
myAll _ [] = True
myAll predicate (x:xs) = (predicate x) && (myAll predicate xs)

myOr :: (a->Bool) -> [a] -> Bool
myOr _ [] = True
myOr predicate (x:xs) = (predicate x) || (myOr predicate xs) 

myTake :: Int -> [a] -> [a]
myTake _ [] = []
myTake count (x:xs) | count >0 = x : myTake (count -1) xs
		    | otherwise = []

mySplitAt :: Int -> [a] -> ([a],[a])
mySplitAt _ [] = ([],[])
mySplitAt count (x:xs) | count > 0 = let splitTuple = mySplitAt (count-1) xs
				     in (x:fst(splitTuple),snd(splitTuple))
		       | otherwise = ([],x:xs)

mySpan :: (a -> Bool) -> [a] -> ([a],[a])
mySpan _ [] = ([],[])
mySpan predicate (x:xs) | predicate x = let splitTuple = mySpan predicate xs
					in (x:fst(splitTuple),snd(splitTuple))
			| otherwise = ([],x:xs) 

myZipWith _ [] _ = []
myZipWith _ _ [] = []
myZipWith func (x:xs) (y:ys) = (func x y) : (myZipWith func xs ys)

safeHead :: [a] -> Maybe a
safeHead [] = Nothing
safeHead (x:xs) = Just x

