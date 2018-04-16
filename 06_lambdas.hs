-- sum of list using foldl
sum' :: (Num a) => [a] -> a
sum' = foldl (+) 0

-- elem checks if element is in list
elem' :: (Eq a) => a -> [a] -> Bool
elem' a = foldl (\acc x -> acc || x == a) False

-- map using foldr
map' :: (a -> b) -> [a] -> [b]
map' f = foldr (\x acc -> (f x):acc) []

-- foldl1 and foldr1 are same as foldl and foldr except
-- they dont' need starting value
-- they start with two elements from list
sum1 :: (Num a) => [a] -> a
sum1 = foldl1 (+)
-- point to note that since these work with two elems
-- they cause error when called with empty lists

-- stdlib functions using fold
maximum' :: (Num a, Ord a) => [a] -> a
maximum' = foldl (max) 0

reverse' :: [a] -> [a]
reverse' = foldl (\acc x -> x:acc) []

product' :: (Num a) => [a] -> a
product' = foldl (*) 1

filter' :: (a -> Bool) -> [a] -> [a]
filter' f = foldl (\acc x -> if (f x) then x:acc else acc) []

head' :: [a] -> a
head' = foldr1 (\x _ -> x)

last' :: [a] -> a
last' = foldl1 (\_ x -> x)

-- how many elements does it take for the sum of sqroots of all natural numbers
-- to exceed 1000
sqrtSums :: Int
sqrtSums = length (takeWhile (<1000) (scanl1 (+) (map sqrt [1..]))) + 1


-- function application using ($) is right associative
-- as opposed to normal function call which is left associative
sqrtApp :: Float
sqrtApp = sqrt $ 3 + 3 + 3 -- returns 3.0 (sqrt 9)

-- function composition with (.)
mapNeg :: [Int]
mapNeg = map (negate . abs) [5, -3, -6, 9, -2, 1]
-- function composition is right associative
