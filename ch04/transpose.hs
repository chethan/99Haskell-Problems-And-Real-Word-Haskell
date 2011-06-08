transpose :: [[a]] -> [[a]]
transpose xs | null xs = []
	     | any null xs = []
	     | otherwise = (map head xs):(transpose (map tail xs))

