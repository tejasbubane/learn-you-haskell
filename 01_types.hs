removeNonUpperCase :: String -> String
removeNonUpperCase cs = [c | c <- cs, c `elem` ['A'..'Z']]

addThree :: Int -> Int -> Int -> Int
addThree x y z = x + y + z

circumference :: Double -> Double
circumference r = 2 * pi * r
