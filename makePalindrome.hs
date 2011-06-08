palindrome (x) = x == reverse x
make_palindrome num = let check_palindrome x = (palindrome$show$(num+x)) || (palindrome$show$(num-x)) 
					  in head $ filter check_palindrome [0..]  
