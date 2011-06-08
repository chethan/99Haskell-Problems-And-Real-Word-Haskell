type BookNum = Int
type BookName = String
type Authors = [String]
data MyInfo = Info{
		 bookNum :: BookNum,
		 bookName:: BookName,
		 authors :: Authors
	         }deriving (Show)	
data MyBool = True | False
	      deriving (Show)
data Mayd a = Just a | Nothing
	       deriving (Show)	
data List a = Cons a (List a)|Nil
		deriving (Show)
formList (x:xs) = Cons x (formList(xs))
formList [] = Nil
toList (Cons a as) = a:toList as
toList Nil = []

