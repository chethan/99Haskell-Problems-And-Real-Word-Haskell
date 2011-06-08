data Tree a = Node a (Tree a) (Tree a)
	    | Empty
	      deriving (Show)

height Empty = 0
height (Node a tree1 tree2) = let treeHeight1 = (height tree1)
				  treeHeight2 = (height tree2)
			      in if treeHeight1 > treeHeight2
			         then 1 + treeHeight1
			         else 1 + treeHeight2	  	   
