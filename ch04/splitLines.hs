splitLines :: String -> [String]
splitLines [] = []
splitLines cs = let (pre,sub) = break isLineTerminator cs
		in pre: case sub of
			   ('\r':'\n':remaining) -> splitLines remaining 
			   ('\r':remaining) -> splitLines remaining
			   ('\n':remaining) -> splitLines remaining
			   _ -> []
isLineTerminator c = c =='\r' || c =='\n'
