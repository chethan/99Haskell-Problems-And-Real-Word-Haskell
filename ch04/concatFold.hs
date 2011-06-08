concatFold :: [[a]] -> [a]
concatFold xs = foldl (++) [] xs
