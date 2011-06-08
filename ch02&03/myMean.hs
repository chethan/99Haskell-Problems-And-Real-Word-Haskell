myMean (xs) = let myLocalMean len (a:as) = (a/fromIntegral(len)) + (myLocalMean len as)
                  myLocalMean _ [] = 0
			  in myLocalMean (length xs) xs