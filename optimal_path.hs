import Data.List

data Section = Section { pathA :: Int, pathB :: Int, pathC :: Int } deriving (Show)
type RoadSystem = [Section]

heathrowToLondon :: RoadSystem
heathrowToLondon = [Section 50 10 30, Section 5 90 20,
                    Section 40 2 25, Section 10 8 0]

data Label = A | B | C | X deriving (Show)
type Path = [(Label, Int)]

roadStep :: (Path, Path) -> Section -> (Path, Path)
roadStep (pathA, pathB) (Section a b c) =
  let costA = sum $ map snd pathA
      costB = sum $ map snd pathB
      directAtoA = costA + a
      crossBtoA = costB + b + c
      directBtoB = costB + b
      crossAtoB = costA + a + c
      newPathToA = if directAtoA <= crossBtoA
                   then (A, a):pathA
                   else (C, c):(B, b):pathB
      newPathToB = if directBtoB <= crossAtoB
                   then (B, b):pathB
                   else (C, c):(A,a):pathA
  in (newPathToA, newPathToB)

optimal :: RoadSystem -> Path
optimal roadSystem =
  let (pathA, pathB) = foldl roadStep ([], []) roadSystem
      costA = sum $ map snd pathA
      costB = sum $ map snd pathB
  in if costA <= costB
     then reverse pathA
     else reverse pathB

groupsOf :: Int -> [a] -> [[a]]
groupsOf 0 _ = undefined
groupsOf _ [] = []
groupsOf n xs = take n xs : groupsOf n (drop n xs)

main = do
  contents <- getContents
  let threes = groupsOf 3 (map read $ lines contents)
      roadSystem = map (\[a, b, c] -> Section a b c) threes
      optimalPath = optimal roadSystem
      pathString = concat $ map (show . fst) optimalPath
      pathPrice = sum $ map snd optimalPath
  putStrLn $ "The best path to take is: " ++ pathString
  putStrLn $ "The price is: " ++ show pathPrice

-- Usage cat path.txt | runhaskell optimal_path.hs
