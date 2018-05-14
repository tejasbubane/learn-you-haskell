data Tree a = EmptyTree | Node a (Tree a) (Tree a) deriving (Ord, Eq, Show)

-- Functor is used for making our data-type iterable

-- This is how Functor is defined in stdlib
-- class Functor f where
--   fmap :: (a -> b) -> f a -> f b

-- instantiating functor means implementing fmap on our type
instance Functor Tree where
  fmap f EmptyTree = EmptyTree
  fmap f (Node x leftsub rightsub) =
    Node (f x) (fmap f leftsub) (fmap f rightsub)
