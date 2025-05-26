module Compression.TreeUtils where

import Data.Aeson
import Data.Tree
import GHC.Generics

{- Location represent where a node is in a `Tree`. Location indexing start with 1, conforming to the notion of "the first argument"
-}

newtype Location = Loc [Int] deriving (Show, Eq, Generic, Ord)

instance ToJSON Location

instance FromJSON Location

(<:) :: Int -> Location -> Location
n <: (Loc ns) = Loc (n : ns)

-- | Get the subtree with given location
navigateTo :: Location -> Tree a -> Tree a
navigateTo (Loc []) t = t
navigateTo (Loc (n : ns)) (Node _ ts) = navigateTo (Loc ns) (ts !! (n - 1))

-- | Plug a tree at a given location in a tree
plugAt :: Location -> Tree a -> Tree a -> Tree a
plugAt (Loc []) _ t' = t'
plugAt (Loc (n : ns)) (Node x ts) t' = Node x (take (n - 1) ts ++ [plugAt (Loc ns) (ts !! (n - 1)) t'] ++ drop n ts)

-- | Perform a tree transformation at a given location in a tree
applyAt :: (Tree a -> Tree a) -> Location -> Tree a -> Tree a
applyAt f loc t = plugAt loc t (f (navigateTo loc t))

-- | Find all the locations within a tree satisfying a predicate. The locations are in post order.
findSlots :: (a -> Bool) -> Tree a -> [Location]
findSlots p (Node x ts) = concat [(i <:) <$> findSlots p t' | (i, t') <- zip [1 ..] ts] ++ [Loc [] | p x]

leafLocations :: Tree a -> [Location]
leafLocations (Node _ []) = [Loc []]
leafLocations (Node _ ts) = concat [(i <:) <$> leafLocations t | (i, t) <- zip [1 ..] ts]

indexToLocation :: Int -> Tree a -> Location
indexToLocation n t = leafLocations t !! n


sizeTree :: Tree a -> Int 
sizeTree = foldTree $ \_ bs -> 1 + sum bs