import Data.Char (isUpper)
capCount = length.filter(isUpper . head ).words  
