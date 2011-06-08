sort (x:[]) = x:[]
sort [] = [] 
sort (xs) =let halfLength = floor((fromIntegral(length xs))/2)  
	       merge([],bs) = bs
	       merge(as,[]) = as
	       merge (a:as,b:bs) | a <=b = a:merge(as,b:bs)
				 | a > b = b:merge(a:as,bs)
	
	   in merge (sort(take halfLength xs), sort(drop halfLength xs)) 
