-- import Data.List -- normal import - use functions directly
-- import Data.List (num) -- selective import
import qualified Data.List as M -- named and qualified import

-- import Geometry -- defined in Geometry.hs - example of single mode
-- files being imported have to be in same folder

-- examples of separate (nested) modules
-- all of these export same methods (area and volume) hence must use qualified
import qualified Geometry.Sphere as Sphere
import qualified Geometry.Cube as Cube
import qualified Geometry.Cuboid as Cuboid

numUniques :: (Eq a) => [a] -> Int
numUniques = length . (M.nub)

-- find key in an associative list - [(k,v)]
findKey :: (Eq k) => k -> [(k, v)] -> Maybe v
findKey _ [] = Nothing
findKey key ((k,v):xs) = if key == k
                          then Just v
                          else findKey key xs

a1 = Sphere.area 2.3
v1 = Sphere.volume 2.3

a2 = Cube.area 1.2
v2 = Cube.volume 1.2

a3 = Cuboid.area 3.4 1.2 2.3
v3 = Cuboid.volume 3.4 1.2 2.3
