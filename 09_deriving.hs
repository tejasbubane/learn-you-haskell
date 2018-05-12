data Day =
  Monday
  | Tuesday
  | Wednesday
  | Thursday
  | Friday
  | Saturday
  | Sunday
  deriving (Eq, Ord, Show, Read, Bounded, Enum)

-- Bouded
minDay :: Day
minDay = minBound :: Day

maxDay :: Day
maxDay = maxBound :: Day

-- Enum
sunday :: Day
sunday = succ Saturday

allDays :: [Day]
allDays = [minDay..maxDay]


-- Read
monday :: Day
monday = read "Monday"

-- Ord
d1 :: Bool
d1 = Wednesday > Tuesday -- True
