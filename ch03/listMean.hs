
listMean (x:xs,length) = (x/length) + listMean(xs,length)
listMean (_,_) = 0
