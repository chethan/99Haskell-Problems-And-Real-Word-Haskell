import System.Environment (getArgs)

interactWith function inputFile outputFile = do
	input <- readFile inputFile
	writeFile outputFile (function input)

main = let mainWith function = do
		 args <- getArgs
		 case args of
		     [input,output] -> interactWith function input output
		     _	-> putStrLn "error:exactlyThreeArgumentsNeeded"
       in mainWith euler13
