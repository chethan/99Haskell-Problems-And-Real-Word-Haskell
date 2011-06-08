myFoldl' _ acc [] = acc
myFoldl' step acc (x:xs) = let temp = step acc x
			   in temp `seq` myFoldl' step temp xs  
