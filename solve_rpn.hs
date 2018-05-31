import Data.List

solve :: String -> Float
solve = head . foldl process [] . words

process :: [Float] -> String -> [Float]
process (x:y:ys) "*" = (x * y):ys
process (x:y:ys) "+" = (x + y):ys
process (x:y:ys) "-" = (y - x):ys
process (x:y:ys) "/" = (y / x):ys
process (x:y:ys) "^" = (y ** x):ys
process acc "sum" = [sum acc]
process acc number = (read number):acc
