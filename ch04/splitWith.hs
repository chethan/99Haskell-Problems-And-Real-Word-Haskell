splitWith _ [] = [[]]
splitWith _ (x:[]) = [[x]]
splitWith (func) (x:xs) | (func x)  =  
						| otherwise = 