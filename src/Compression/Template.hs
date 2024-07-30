module Compression.Template where

import Data.List.Split
import Data.Tree
import Compression.Meta

data Template a
  = Template a
  | Comp Int (Template a) (Template a)
  | WithRep (Template a) Meta [Template a]
  deriving (Show, Eq, Functor)

instance Applicative Template where
  pure = Template

isTemplate :: Template a -> Bool
isTemplate = \case
  Template {} -> True
  _ -> False

inputsT :: (a -> b -> [b]) -> b -> Template a -> [b]
inputsT f r = \case
  Template x -> f x r
  Comp i x y -> take i inputX ++ inputY ++ drop (i + 1) inputX
    where
      inputX = (inputsT f r x)
      inputY = inputsT f r' y
      r' = inputX !! i
  WithRep x m xs -> mconcat $ zipWith (inputsT f) rs xs'
    where
      rs = inputsT f r x
      xs' = useMeta m x xs

data FTree f a = FNode (f a) [FTree f a] deriving (Show, Eq, Functor)

-- data FancyTree a = FNode (Template a) [FancyTree a] deriving (Show,Eq,Functor)

type FancyTree a = FTree Template a

encode :: Tree a -> FancyTree a
encode (Node x xs) = FNode (Template x) (encode <$> xs)

decode :: (a -> Int) -> FancyTree a -> Tree a
decode f (FNode tp ts) = case tp of
  Template x -> Node x (decode f <$> ts)
  t@(Comp i x y) -> decode f $ FNode x ts'
    where
      ts' = l ++ [FNode y m] ++ r
      [l, m, r] = compSplit i (arityT f t) (arityT f y) ts
  WithRep x m xs -> decode f $ FNode x ts'
    where
      ts' = zipWith FNode fs args
      fs = useMeta m x xs
      args = splitPlaces (arityT f <$> fs) ts

arityT :: (a -> Int) -> Template a -> Int
arityT arity = \case
  Template x -> arity x
  Comp i x y -> arityT arity x + arityT arity y - 1
  WithRep x m xs -> sum (arityT arity <$> useMeta m x xs)

compSplit :: (Integral a) => a -> a -> a -> [b] -> [[b]]
compSplit i n k = splitPlacesBlanks [i, k, n - i - k]

testTree :: Tree Int
testTree = Node 3 [Node 5 [], Node 7 []]

fLeaf x = FNode (pure x) []

testFTree =
  FNode
    (Comp 1 (Template 'f') (Template 'g'))
    [fLeaf 'z', fLeaf 'z', fLeaf 'z', fLeaf 'z']

-- >>> decode . encode $ testTree
-- Node {rootLabel = 3, subForest = [Node {rootLabel = 5, subForest = []},Node {rootLabel = 7, subForest = []}]}

-- >>>  decode  (\case 'f' -> 3 ; 'g' -> 2) $ testFTree
-- Node {rootLabel = 'f', subForest = [Node {rootLabel = 'z', subForest = []},Node {rootLabel = 'g', subForest = [Node {rootLabel = 'z', subForest = []},Node {rootLabel = 'z', subForest = []}]},Node {rootLabel = 'z', subForest = []}]}
