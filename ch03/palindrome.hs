palindrome (x:[]) = True
palindrome [] = True
palindrome (x) = x == reverse x

