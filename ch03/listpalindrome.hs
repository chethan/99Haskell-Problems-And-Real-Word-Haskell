listPalindrome (x:xs) = (x:listPalindrome(xs))++(x:[])
listPalindrome _ = []
