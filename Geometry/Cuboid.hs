module Geometry.Cuboid
  ( volume
  , area
  ) where

volume :: Float -> Float -> Float -> Float
volume a b c = rectangleArea a b * c

area :: Float -> Float -> Float -> Float
area a b c = rectangleArea a b * 2 +
             rectangleArea b c * 2 +
             rectangleArea c a * 2

-- you can choose not to export some functions
rectangleArea :: Float -> Float -> Float
rectangleArea a b = a * b
