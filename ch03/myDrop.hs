myDrop n lst = if n <= 0 || null lst
		then lst
		else myDrop (n-1) (tail lst)
