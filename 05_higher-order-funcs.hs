-- Apply function twice
applyTwice :: (a -> a) -> a -> a
applyTwice f x = f (f x)

-- zip two lists using given function
zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith' _ [] _ = []
zipWith' _ _ [] = []
zipWith' f (x:xs) (y:ys) = (f x y):(zipWith f xs ys)

-- flip function arguments
flip' :: (a -> b -> c) -> (b -> a -> c)
flip' f x y = f y x

-- map over a list applying given function to each element
map' :: (a -> b) -> [a] -> [b]
map' _ [] = []
map' f (x:xs) = f x : map f xs

-- filter elements from list using given predicate
filter' :: (a -> Bool) -> [a] -> [a]
filter' _ [] = []
filter' f (x:xs)
  | f(x) = x : filter f xs
  | otherwise = filter f xs

-- quicksort using filter
quicksort :: (Ord a) => [a] -> [a]
quicksort [] = []
quicksort (x:xs) =
  quicksort(filter (<x) xs)
  ++ [x]
  ++ quicksort(filter (>=x) xs)

-- largest number under 100,000 divisible by 3829
largestDivisible :: (Integral a) => a
largestDivisible = head(filter p [100000, 99999..])
  where p x = x `mod` 3829 == 0

-- return elements from list while predicate holds true
takeWhile' :: (a -> Bool) -> [a] -> [a]
takeWhile' _ [] = []
takeWhile' f (x:xs)
  | f(x) = x : (takeWhile' f xs)
  | otherwise = []

-- sum of odd squares that are smaller than 10,000
sumOddSquares :: Int
sumOddSquares = sum (takeWhile (<10000) (filter' odd (map' (^2) [1..])))

-- Collatz sequence: https://en.wikipedia.org/wiki/Collatz_conjecture
collatzChain :: (Integral a) => a -> [a]
collatzChain 1 = []
collatzChain i
  | even i = evenVal : collatzChain(evenVal)
  | odd i = oddVal : collatzChain(oddVal)
    where evenVal = i `div` 2
          oddVal = i * 3 + 1

-- for all starting numbers between 1 and 100,
-- how many collatz chains have length greater than 15
chainGreater :: Int
chainGreater = length (filter (>15) (map length (map collatzChain [1..100])))
