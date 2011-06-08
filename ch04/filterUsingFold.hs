filterr func xs = let step x acc | func x = x : acc
				 | otherwise = acc
		  in foldr step [] xs

filterl func xs = let step acc x | func x = x:acc
				 | otherwise = acc
		  in foldl step [] xs
