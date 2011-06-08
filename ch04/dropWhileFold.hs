dropWhileFold pred xs = let step acc x| pred x  && null acc= []
				      | otherwise = x:acc
			in foldl step [] xs 
