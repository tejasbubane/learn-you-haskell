-- Define custom data-types

module Shapes
( Point (..)
  , Shape
  , surface
  , nudge
  , baseCircle
  , baseRect
) where
-- even though Circle and Rectangle constructors are not exported, users
-- of this module can still create circles using baseCircle/baseRect and nudge

data Point = Point Float Float deriving (Show)
-- Circle with centre coordinates and radius
-- Rectangle with two corner coordinates
data Shape = Circle Point Float | Rectangle Point Point deriving (Show)

surface :: Shape -> Float
surface (Circle _ r) = pi * r ^ 2
surface (Rectangle (Point x1 y1) (Point x2 y2)) =
  (abs $ x2 - x1) * (abs $ y2 - y1)

-- move shapes by given distance in X and Y (a, b)
nudge :: Shape -> Float -> Float -> Shape
nudge (Circle (Point x y) r) a b = Circle (Point (x + a) (y + b)) r
nudge (Rectangle (Point x1 y1) (Point x2 y2)) a b =
  Rectangle (Point (x1 + a) (y1 + b)) (Point (x2 + a) (y2 + b))

baseCircle :: Float -> Shape
baseCircle r = Circle (Point 0 0) r

baseRect :: Float -> Float -> Shape
baseRect width height = Rectangle (Point 0 0) (Point width height)

-- Records
data Person = Person { firstName :: String
                     , lastName :: String
                     , age :: Int
                     , height :: Float
                     , phoneNumber :: String
                     , iceCreamFlavour :: String
                     } deriving (Show)

tejas :: Person
tejas = Person {firstName="Tejas", lastName="Bubane", age=28, height=180, phoneNumber="9595240353", iceCreamFlavour="Butterscotch"}
-- order of parameters does not matter while creating records - advantage

data Car = Car { company :: String
               , model :: String
               , year :: Int } deriving (Show)

mycar :: Car
mycar = Car {company="Honda", model="City", year=2018}

-- Type parameters
data Vector a = Vector a a a deriving (Show)

vplus :: (Num t) => Vector t -> Vector t -> Vector t
(Vector x y z) `vplus` (Vector a b c) = Vector (x+a) (y+b) (z+c)

vmult :: (Num t) => Vector t -> t -> Vector t
(Vector x y z) `vmult` m = Vector (x*m) (y*m) (z*m)

scalarmult :: (Num t) => Vector t -> Vector t -> t
(Vector x y z) `scalarmult` (Vector a b c) = (x*a) + (y*b) + (z*c)
