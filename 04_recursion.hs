-- maximum element from a list
maximum' :: (Ord a) => [a] -> a
maximum' [] = error "No elements"
maximum' (x:[]) = x
maximum' (x:xs)
  | x > maxs = x
  | otherwise = maxs
  where maxs = maximum' xs

-- several repetitions of same element
times :: (Num i, Eq i) => i -> a -> [a]
times 0 _ = []
times n el = el:(times (n - 1) el)

-- takes certain number of elements of a list
take' :: (Num i, Ord i) => i -> [a] -> [a]
take' _ [] = []
take' n (x:xs)
  | n <= 0 = []
  | otherwise = x:(take' (n - 1) xs)

-- reverse list
reverse' :: [a] -> [a]
reverse' [] = []
reverse' (x:xs) = reverse'(xs) ++ [x]

-- repeat given number into an infinite list
repeat' :: a -> [a]
repeat' x = x:(repeat' x)

-- take' 5 repeat 6 works like a charm and returns [6,6,6,6,6]

-- zip two lists
zip' :: [a] -> [b] -> [(a,b)]
zip' [] _ = []
zip' _ [] = []
zip' (x:xs) (y:ys) = (x, y):(zip' xs ys)

-- check if given element is in a list
elem' :: (Eq a) => a -> [a] -> Bool
elem' _ [] = False
elem' el (x:xs) = (el == x) || elem' el xs

-- quicksort!
quicksort :: (Ord a) => [a] -> [a]
quicksort [] = []
quicksort (x:xs) =
  quicksort [y | y <- xs, y <= x]
  ++ [x] ++
  quicksort [z | z <- xs, z > x]
