myIntersperse::a->[[a]]->[a]
myIntersperse _ [] = []
myIntersperse _ (x:[]) = x
myIntersperse sep (x:xs) = x++[sep]++(myIntersperse sep (xs))
