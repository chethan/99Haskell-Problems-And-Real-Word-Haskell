myTakeWhile1 func (x:xs) | func x = x:(myTakeWhile func xs)
						| otherwise = []
						