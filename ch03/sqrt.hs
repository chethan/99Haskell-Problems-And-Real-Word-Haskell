sqroot num = guess_sqrt num num/1 


guess_sqrt number guess = if (abs (number/guess-guess)) < 0.0001 
				then guess
				else guess_sqrt number (((number/guess)+guess)/2)

