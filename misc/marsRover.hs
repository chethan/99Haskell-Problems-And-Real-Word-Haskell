
 findAfter (x:[]) _ =  x
 findAfter (x:y:xs) x =  y
 findAfter (x:y:xs) z = findAfter (y:xs) z
 
 findBefore (x:[]) _ =  x
 findBefore (x:y:xs) z |y == z = x 
                       |otherwise = findBefore (y:xs) z

 findIndex (x:[]) _ = 0
 findIndex (x:xs) y | x ==y = 0
                    | otherwise = 1 + findIndex xs y


 turn currentDir 'R' =  findAfter "NESWN" currentDir
 turn currentDir 'L' =  findBefore "NESWN" currentDir


 move (x,y,currentDir) = (x +([0,1,0,-1] !! ( findIndex "NESW" currentDir )),y+([1,0,-1,0] !! (findIndex "NESW" currentDir)),currentDir)

 roverController (x,y,currentDir) [] = (x,y,currentDir)
 roverController (x,y,currentDir) (command:instructions) |command =='M' = roverController (move (x,y,currentDir)) instructions
                                                         |otherwise = roverController (x,y,(turn currentDir command)) instructions






