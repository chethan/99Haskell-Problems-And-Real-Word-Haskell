safeLast [] = Nothing
safeLast (x:[]) = Just x
safeLast (x:xs) = safeLast xs
