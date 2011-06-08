takeWhileFold ::(a -> Bool) -> [a] -> [a]
takeWhileFold pred xs = let step x acc | pred x = x:acc
				       | otherwise = []
			in foldr step [] xs 
