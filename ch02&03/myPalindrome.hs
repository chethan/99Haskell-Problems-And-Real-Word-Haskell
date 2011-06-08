myPalindrome [] = []
myPalindrome (x:xs) = (x:[])++myPalindrome(xs)++(x:[])
