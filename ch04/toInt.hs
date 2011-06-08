import Data.Char (digitToInt)
toInt :: String -> Int
toInt number = let loop acc (x:xs) = loop (acc*10 + digitToInt x) xs
		   loop acc [] = acc
	       in loop 0 number

toIntFold number = let step acc x = (acc*10 + digitToInt x) 
		   in foldl step 0 number
  

