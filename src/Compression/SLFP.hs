{-# LANGUAGE UndecidableInstances #-}

module Compression.SLFP where

import Compression.Meta (Meta, RepSymbol (..), inferMeta, useMeta)
import Compression.TreeUtils (Location (..), applyAt, fillHoles, findSlots, (<:))
import Control.Applicative (asum)
import Data.Aeson
import Data.Foldable (Foldable (toList))
import Data.Function (on)
import Data.Functor.Base (TreeF (..))
import Data.Functor.Foldable
import Data.List hiding (transpose)
import Data.List.Split (splitPlacesBlanks)
import qualified Data.Map as Map
import Data.Maybe
import Data.Monoid (Sum (getSum))
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Tree (Tree (..))
import GHC.Generics (Generic)
import Prettyprinter (Pretty (..), (<+>))
import qualified Prettyprinter as Pretty
import Safe (maximumByMay)
import Text.Printf (PrintfType, printf)

{- | Accumulated results of applying a list of f :: a->a  on a initial value.
The output list is finite if we can reach a fixpoint of f.
The first element of the output list is the initial value
-}
iterateFs :: (Eq a) => [a -> a] -> a -> [a]
iterateFs x y = iterateFs' x y []

iterateFs' :: (Eq a) => [a -> a] -> a -> [a] -> [a]
iterateFs' [] x acc = acc ++ [x]
iterateFs' (f : fs) x acc
  | x' == x = iterateFs' [] x acc
  | otherwise = iterateFs' fs x' (acc ++ [x])
 where
  x' = f x

fixedPoint :: (Eq t) => (t -> t) -> t -> t
fixedPoint f x
  | f x == x = x
  | otherwise = fixedPoint f (f x)

containsRepeat :: (Foldable t) => t RepSymbol -> Bool
containsRepeat = any (/= New)

matchR :: (Eq a) => Meta -> Tree a -> Bool
matchR m (Node x ts) = m == inferMeta x (rootLabel <$> ts)

data Pattern a
  = Comp Int a a
  | TreePattern (Tree a)
  deriving (Show, Eq, Ord, Generic)

instance (FromJSON a) => FromJSON (Pattern a)
instance (ToJSON a) => ToJSON (Pattern a)

data Abstraction a
  = Constant a
  | Var String
  | (Abstraction a) `With` (String, [Abstraction a])
  | Hole
  deriving (Show, Eq, Ord, Generic)

instance (FromJSON a) => FromJSON (Abstraction a)
instance (ToJSON a) => ToJSON (Abstraction a)
instance (ToJSON a) => ToJSONKey (Abstraction a)
instance (FromJSON a) => FromJSONKey (Abstraction a)

instance (Pretty a) => Pretty (Abstraction a) where
  pretty (Hole) = "◯"
  pretty (Constant x) = pretty x
  pretty (Var x) = pretty x
  pretty (x `With` (s, xs)) =
    pretty x <+> "{"
      <> pretty s
      <> "}"
        <+> Pretty.hsep (pretty <$> xs)

instance Size (Abstraction a) where
  size = \case
    Constant _ -> 1
    Var _ -> 1
    _ `With` (_, xs) -> 2 + length xs
    Hole -> 0

getVariables :: Abstraction a -> Set String
getVariables = \case
  Constant _ -> mempty
  Hole -> mempty
  Var x -> Set.singleton x
  x `With` (_, ys) -> foldMap getVariables (x : ys)

getPatternOrRules :: (_) => Abstraction r -> Set (Either r String)
getPatternOrRules = \case
  Constant x -> Set.singleton $ Left x
  Var x -> Set.singleton $ Right x
  x `With` (_, ys) -> foldMap getPatternOrRules (x : ys)
  Hole -> mempty

class a `CachedBy` b where
  retrieve :: Map.Map b a

getMeta :: (Eq a) => Tree a -> Meta
getMeta t = inferMeta (rootLabel t) (rootLabel <$> subForest t)

-- newtype Location = Loc [Int] deriving (Show, Eq)

-- (<:) :: Int -> Location -> Location
-- n <: Loc ns = Loc (n : ns)

pRewriteAll :: (a, Pattern a) -> Tree a -> [Location] -> Tree a
pRewriteAll e = applyPostOrder (pRewriteTop e)

rRewriteAll ::
  (String, Meta) ->
  Tree (Abstraction a) ->
  [Location] ->
  Tree (Abstraction a)
rRewriteAll e = applyPostOrder (rRewriteTop e)

pSubstituteAll :: (_) => Int -> Pattern (Abstraction a) -> Tree (Abstraction a) -> [Location] -> Tree (Abstraction a)
pSubstituteAll n e = applyPostOrder (pSubstitutionTop n e)

rSubstituteAll :: (Abstraction a, [Int], Meta) -> Tree (Abstraction a) -> [Location] -> Tree (Abstraction a)
rSubstituteAll e = applyPostOrder (rSubstitutionTop e)

{- |
length of the meta rule must coincide with the length of the forest.
<New,1,New,3,Star> and [t1, t2, t3, t4, t5]
    = [t1] ++ subforest t2 ++ [t3] ++ subforest t4 ++ subforest t5
    = f New t1 ++ (f 1 t2) ++ ...
    = foldMap g ts where g = ...
-}
rRewriteTop :: (String, Meta) -> Tree (Abstraction a) -> Tree (Abstraction a)
rRewriteTop (x, m) (Node r ts) = Node (r `With` (x, catMaybes $ zipWith pickByMeta m (rootLabel <$> ts))) (foldMap subForest ts)

pickByMeta :: RepSymbol -> a -> Maybe a
pickByMeta New a = Just a
pickByMeta _ _ = Nothing

rSubstitutionTop :: (Abstraction a, [Int], Meta) -> Tree (Abstraction a) -> Tree (Abstraction a)
rSubstitutionTop (rt `With` (_, abs), ns, m) (Node _ ts) = Node rt ts'
 where
  ts' = zipWith Node (useMeta m rt abs) (splitPlacesBlanks ns ts)

toRankLabeled :: Tree a -> Tree (a, Int)
toRankLabeled (Node x ts) = Node (x, length ts) (toRankLabeled <$> ts)

arityMap :: (Ord a) => Tree a -> Map.Map a Int
arityMap = Map.fromList . toList . toRankLabeled

-- | apply a single rewrite on a tree according to a pattern and its symbol.
pRewriteTop :: (a, Pattern a) -> Tree a -> Tree a
pRewriteTop (x, Comp i _ _) (Node _ ts) = Node x $ take (i - 1) ts ++ subForest (ts !! (i - 1)) ++ drop i ts

-- | inverse of pRewriteTop.
pSubstitutionTop :: (_) => Int -> Pattern (Abstraction a) -> Tree (Abstraction a) -> Tree (Abstraction a)
pSubstitutionTop n (Comp i r y) (Node _ ts) = Node r $ take (i - 1) ts ++ [Node y (take n . drop (i - 1) $ ts)] ++ drop (i + n - 1) ts
pSubstitutionTop n (TreePattern t) y@(Node _ ts)
  = fillHoles (== Hole) t ts
  -- | length ts == n = fillHoles (== Hole) t ts
  -- | otherwise = error $ "error in pSubstitutionTop: arity not matching arguments " <> show (n,t, y)

-- | Given a tree transformation function, a list of locations in post order, apply the transformation to all location in the tree
applyPostOrder :: (Tree a -> Tree a) -> Tree a -> [Location] -> Tree a
applyPostOrder _ t [] = t
applyPostOrder f t (l : ls) = applyPostOrder f (applyAt f l t) ls

{- |
given a function that update the unioned location (wrt current root) dictionary of subtrees with a tree (the inductive step), induce a function that output
dictionary of (symbol,locations).
-}
occurrences :: (Ord k) => (Tree a -> Map.Map k [Location] -> Map.Map k [Location]) -> Tree a -> Map.Map k [Location]
occurrences f t =
  f t . Map.unionsWith (++) $
    [Map.map (changeCoord i) (occurrences f t') | (i, t') <- zip [1 ..] (subForest t)]
 where
  changeCoord i = fmap (i <:)

occurrencesG ::
  (Ord k) =>
  (Tree a -> Map.Map k [Location] -> Map.Map k [Location]) ->
  Map.Map b (Tree a) ->
  Map.Map b (Map.Map k [Location])
occurrencesG f = fmap (occurrences f)

-- | output a dictionary (Meta,Locations) from a tree
occR :: (_) => Tree a -> Map.Map Meta [Location]
occR = Map.filterWithKey (\m v -> containsRepeat m) . occurrences addTopMeta

addTopMeta :: (Eq a) => Tree a -> Map.Map Meta [Location] -> Map.Map Meta [Location]
addTopMeta t m = case Map.lookup rootMeta m of
  Nothing -> Map.insertWith (flip (++)) rootMeta [Loc []] m
  Just locs
    | any (`elem` dangerousLocs) locs -> m
    | otherwise -> Map.insertWith (flip (++)) rootMeta [Loc []] m
 where
  rootMeta = getMeta t
  dangerousLocs = [Loc [i] | (i, x) <- (zip [1 ..] rootMeta), x /= New]

-- | output a dictionary (Pattern,Locations) from a tree
occP :: (Ord a) => Tree a -> Map.Map (Pattern a) [Location]
occP = occurrences addTopDigrams

addTopDigrams ::
  (Ord a) =>
  Tree a ->
  Map.Map (Pattern a) [Location] ->
  Map.Map (Pattern a) [Location]
addTopDigrams (Node x ts) = foldr (.) id ([updateEntry i d | (i, d) <- zip [1 ..] ps])
 where
  ps = [Comp i x (rootLabel t) | (i, t) <- zip [1 ..] ts]
  updateEntry i d m =
    Map.insertWithKey
      (\k _ v -> v ++ [Loc [] | Loc [i] `notElem` m Map.! k]) -- for existing digrams, we only add new location (which is the root) when it does not overlap with any of the existing locations (maybe BUG in non-overlapping property)
      d
      [Loc []] -- for fresh digrams, their only location is Loc []
      m

-- | output two dictionaries (Pattern,Locations), (Meta,Locations) from a tree
occAll :: (_) => Tree a -> (Map.Map (Pattern a) [Location], Map.Map Meta [Location])
occAll t = (occP t, occR t)

data SLTP a = SLTP
  { compressedTree :: Tree (Abstraction a)
  , patterns :: Map.Map String (Pattern (Abstraction a))
  , metas :: Map.Map String Meta
  }
  deriving (Show, Eq, Generic)

instance (ToJSON a, ToJSONKey a) => ToJSON (SLTP a)
instance (FromJSON a, FromJSONKey a) => FromJSON (SLTP a)

initSLTP :: Tree a -> SLTP a
initSLTP t = SLTP (Constant <$> t) Map.empty Map.empty

compressL :: (Ord a) => Int -> p -> Map.Map String Meta -> SLTP a -> SLTP a
compressL n dpG drG g@(SLTP t dP dM) = case (mP, mR) of
  (Nothing, Nothing) -> g
  (Just (p, (plocs, savP)), Nothing)
    | length plocs > 1 -> performPRewrite n p plocs g
    | otherwise -> g
  (Nothing, Just (r, (rlocs, savR)))
    | length rlocs > 1 -> performRRewrite n drG r rlocs g
    | otherwise -> g
  (Just (p, (plocs, savP)), Just (r, (rlocs, savR)))
    | savR >= savP && length rlocs > 1 -> performRRewrite n drG r rlocs g
    | length plocs > 1 -> performPRewrite n p plocs g
    | otherwise -> g
 where
  (oP, oR) = occAll t
  mP = bestAbstractionL (Map.mapWithKey (\k v -> (v, k `elem` dP)) oP)
  mR = bestAbstractionL (Map.mapWithKey (\k v -> (v, k `elem` dM)) oR)

performPRewrite :: (Ord a) => Int -> Pattern (Abstraction a) -> [Location] -> SLTP a -> SLTP a
performPRewrite n p locs (SLTP t dP dR) = SLTP t' dP' dR'
 where
  (name :: String) = printf "P%d_%d" n (Map.size dP + 1)
  t' = pRewriteAll (Var name, p) t locs
  (dP', dR') = (Map.insert name p dP, dR)

performRRewrite :: (Ord a) => Int -> Map.Map String Meta -> Meta -> [Location] -> SLTP a -> SLTP a
performRRewrite n drG m locs (SLTP t dP dR) = SLTP t' dP dR'
 where
  name
    | m `elem` drG = lookupByValue m drG
    | m `elem` dR = lookupByValue m dR
    | otherwise = printf "R%d_%d" n (Map.size dR + 1)

  t' = rRewriteAll (name, m) t locs
  dR'
    | not (m `elem` drG || m `elem` dR) = Map.insert name m dR
    | otherwise = dR

class Size a where
  size :: a -> Int

instance (Size (Abstraction a)) => Size (SLFP a b) where
  size x = sum (size <$> sltps x) + size (globalPatterns x) + size (globalMetas x)

instance (Size (Abstraction a)) => Size (SLTP a) where
  size (SLTP t pD mD) = size t + size pD + size mD

instance (Size a) => Size (Tree a) where
  size t = sum $ size <$> t

instance (Size b) => Size (Map.Map a b) where
  size m = getSum $ foldMap (fromIntegral . size) m

instance (Size a) => Size (Pattern a) where
  size = \case 
    Comp{} ->  3
    TreePattern t -> sum $ size <$> t

instance Size Meta where
  size = length

class (Size a) => Compressor a where
  unitSave :: a -> Int

  cost :: a -> Int
  cost = size

  totalSave :: a -> Int -> Bool -> Int
  totalSave x n stored = n * unitSave x - (if stored then 0 else cost x)

instance Compressor Meta where
  unitSave = length . filter (/= New)

instance Size a => Compressor (Pattern a) where
  unitSave = const 1

evaluateAbstraction ::
  (Compressor a, Foldable t) =>
  Map.Map a (t b, Bool) ->
  Map.Map a (t b, Int)
evaluateAbstraction = Map.mapWithKey (\x (locs, stored) -> (locs, totalSave x (length locs) stored))

evaluateAbstractionG :: (Compressor a, Foldable t, _) => Map.Map a (Map.Map k (t b), Bool) -> Map.Map a (Map.Map k (t b), Int)
evaluateAbstractionG = Map.mapWithKey (\x (locs, stored) -> (locs, totalSave x (length $ mconcat (Map.elems locs)) stored))

bestAbstractionL :: (Compressor a, Foldable t) => Map.Map a (t b, Bool) -> Maybe (a, (t b, Int))
bestAbstractionL d = maximumByMay (compare `on` \(_, (_, x)) -> x) . Map.toList $ evaluateAbstraction d

bestAbstractionG ::
  (Compressor a, Foldable t, _) =>
  Map.Map a (Map.Map k (t b), Bool) ->
  Maybe (a, (Map.Map k (t b), Int))
bestAbstractionG d =
  maximumByMay (compare `on` \(_, (_, x)) -> x)
    . filter (\(_, (x, _)) -> Map.size x > 1)
    . Map.toList
    $ evaluateAbstractionG d

transpose :: (Ord k1, Ord k2) => Map.Map k1 (Map.Map k2 a) -> Map.Map k2 (Map.Map k1 a)
transpose table =
  let foo = Map.toList . fmap Map.toList $ table
      bar = concatMap (\(a, bvs {-[(b,v)]-}) -> zip (repeat a) bvs) foo
      baz = (\(a, (b, v)) -> (b, a, v)) <$> bar
   in fromList2 baz

-- fromList2 :: (Ord a, Ord b) => [(a,b,v)] -> Lookup2 a b v
fromList2 :: (Ord k1, Ord k2) => [(k2, k1, a)] -> Map.Map k2 (Map.Map k1 a)
fromList2 xs =
  let ys = (\(a, b, v) -> (a, [(b, v)])) <$> xs
   in fmap Map.fromList $ Map.fromListWith (++) ys

data SLFP a b = SLFP
  { sltps :: Map.Map b (SLTP a)
  , globalPatterns :: Map.Map String (Pattern (Abstraction a))
  , globalMetas :: Map.Map String Meta
  , arities :: Map.Map (Abstraction a) Int
  }
  deriving (Show, Eq, Generic)

instance (ToJSON a, ToJSONKey a, ToJSON b, ToJSONKey b, ToJSONKey (Abstraction a)) => ToJSON (SLFP a b)
instance (Ord a, Ord b, FromJSON a, FromJSONKey b, FromJSONKey a) => FromJSON (SLFP a b)

initSLFP :: (_) => [(b, Tree a)] -> SLFP a b
initSLFP nts =
  SLFP
    (Map.fromList [(n, initSLTP t) | (n, t) <- nts])
    Map.empty
    Map.empty
    (foldMap (arityMap . fmap Constant . snd) nts)

localMetas :: (_) => SLFP a b -> Map.Map String Meta
localMetas slfp = foldMap metas (sltps slfp)

globalOcc ::
  (Ord pc, Ord pat) =>
  -- | a tree pattern locator, extract a collection of patterns and their occurances (locations) in a tree
  (Tree (Abstraction a) -> Map.Map pat loc) ->
  -- | an indexed collection of trees
  Map.Map pc (Tree (Abstraction a)) ->
  -- | collection of patterns with their piecewise occurances (locations)
  Map.Map pat (Map.Map pc loc)
globalOcc f dE = transpose $ f <$> dE

alterTree :: (Tree (Abstraction a) -> Tree (Abstraction a)) -> SLTP a -> SLTP a
alterTree f (SLTP t dP dM) = SLTP (f t) dP dM

freshSymbol :: String -> p -> Map.Map k1 a1 -> Map.Map k2 a2 -> String
freshSymbol s dE dpG drG = printf "%s%d" s $ Map.size dpG + Map.size drG + 1

zipDict :: (Ord k) => Map.Map k a1 -> Map.Map k [a2] -> Map.Map k (a1, [a2])
zipDict d1 d2 = Map.mapWithKey f d1
 where
  f k v = case Map.lookup k d2 of
    Nothing -> (v, [])
    Just v' -> (v, v')

rewriteTreeOnly rewrite (symb, r) = alterTree . flip (rewrite (symb, r))

pRewriteTreeOnly :: (Abstraction a, Pattern (Abstraction a)) -> [Location] -> SLTP a -> SLTP a
pRewriteTreeOnly = rewriteTreeOnly pRewriteAll

rRewriteTreeOnly :: (String, Meta) -> [Location] -> SLTP a -> SLTP a
rRewriteTreeOnly = rewriteTreeOnly rRewriteAll

insertWhenNotElement :: (Ord k, Eq a) => a -> Map.Map k a -> k -> Map.Map k a
insertWhenNotElement v d newName = if v `elem` d then d else Map.insert newName v d

lookupByValue :: (Eq a) => a -> Map.Map b a -> b
lookupByValue v d = head [k | (k, v') <- Map.assocs d, v == v']

compressG :: (Ord b, Ord a) => SLFP a b -> SLFP a b
compressG g@(SLFP dE dpG drG dA) = case (mp, mr) of
  (Nothing, Nothing) -> compressLocally g
  (Just (p, (plocs, savP)), Nothing) -> compressByP p plocs
  (Nothing, Just (r, (rlocs, savR))) -> compressByR r rlocs
  (Just (p, (plocs, savP)), Just (r, (rlocs, savR))) ->
    compressByP p plocs
 where
  -- \| savP > savR -> compressByP p plocs
  -- \| otherwise -> compressByR r rlocs

  -- compressByP p plocs -- just an idea: do p rewrite when possible to make more oppotunity of r-rewrite on large pattern

  compressLocally (SLFP x y z a) =
    SLFP
      (Map.fromList $ [(p, compressL i dpG drG e) | (i, (p, e)) <- zip [1 ..] (Map.toList x)])
      y
      z
      a
  compressByP p plocs =
    SLFP
      (uncurry (flip $ pRewriteTreeOnly (Var $ nameP p, p)) <$> zipDict dE plocs)
      (insertWhenNotElement p dpG $ nameP p)
      drG
      dA
  compressByR r rlocs =
    SLFP
      (uncurry (flip $ rRewriteTreeOnly (nameR r, r)) <$> zipDict dE rlocs)
      dpG
      (insertWhenNotElement r drG $ nameR r)
      dA
  mp = bestAbstractionG (Map.mapWithKey (\k v -> (v, k `elem` dpG)) (globalOcc occP $ compressedTree <$> dE))
  mr = bestAbstractionG (Map.mapWithKey (\k v -> (v, k `elem` drG)) (globalOcc occR $ compressedTree <$> dE))
  nameP p = if p `elem` dpG then lookupByValue p dpG else freshSymbol "PG" dE dpG drG
  nameR r = if r `elem` drG then lookupByValue r drG else freshSymbol "RG" dE dpG drG

compressGSteps :: (Ord b, _) => SLFP r b -> [SLFP r b]
compressGSteps slfp = iterateFs [compressG | _ <- [0 ..]] slfp

deCompressG :: (_) => SLFP a b -> SLFP a b
deCompressG g@(SLFP dE dpG drG dA) = g{sltps = dE'}
 where
  dE' = deCompressL dpG drG dA <$> dE

deCompressL ::
  (Ord a, _) =>
  Map.Map String (Pattern (Abstraction a)) ->
  Map.Map String Meta ->
  Map.Map (Abstraction a) Int ->
  SLTP a ->
  SLTP a
deCompressL dpG drG dA (SLTP t dP dM) = SLTP t' dP dM
 where
  t' = deCompressTree (debugLookup (dpG <> dP) ) (debugLookup (drG <> dM)) (debugLookup dA ) t

debugLookup :: (Ord k, Show k, Show a) => Map.Map k a -> k -> a
debugLookup d k = case Map.lookup k d of
  Nothing -> error $ "(debugLookUp) key not found: " <> show k
  Just v -> v

deCompressTree ::
  (Ord a, _) =>
  (String -> Pattern (Abstraction a)) ->
  (String -> Meta) ->
  (Abstraction a -> Int) ->
  Tree (Abstraction a) ->
  Tree (Abstraction a)
deCompressTree dP dM dA t@(Node r ts) = case r of
  Constant{} -> Node r (deCompressTree dP dM dA <$> ts)
  Var x -> pSubstituteAll n pat t (findSlots (== r) t)
   where
    pat = dP x
    n = case pat of
      Comp _ _ y -> inferArity dP dM dA y
      TreePattern t' -> numHoles t'
  x `With` (m, as) ->
    -- \| temporary disable to locate branching hole error (which is caused by rSubstituteAll)
    rSubstituteAll
      (r, groupings, meta)
      t
      (findSlots (== r) t)
   where
    groupings = inferArity dP dM dA <$> useMeta meta x as
    meta = dM m
  Hole -> case ts of
    [] -> t
    _ -> error $ "deCompressTree: Hole found in the branching node, this should not happen. The problematic tree is " <> show t

numHoles :: Tree (Abstraction a) -> Int
numHoles = cata $ \case
  NodeF Hole [] -> 1
  NodeF _ [] -> 0
  NodeF _ ns -> sum ns

inferArity ::
  (String -> Pattern (Abstraction a)) ->
  (String -> Meta) ->
  (Abstraction a -> Int) ->
  Abstraction a ->
  Int
inferArity d dMeta dA a = case a of
  Constant x -> dA (Constant x)
  Var x -> case d x of
    Comp _ l r -> inferArity d dMeta dA l + inferArity d dMeta dA r - 1
    TreePattern t -> numHoles t
  a' `With` (m, as) -> sum (inferArity d dMeta dA <$> useMeta (dMeta m) a' as)
  Hole -> 1

symbolToExpandTop :: Abstraction a -> Maybe (Abstraction a)
symbolToExpandTop a = case a of
  Constant _ -> Nothing
  _ -> Just a

symbolToExpand :: Tree (Abstraction a) -> Maybe (Abstraction a)
symbolToExpand t = asum (symbolToExpandTop <$> t)

-- >>> asum (Node (Just 'a') [Node (Just 'b') [],Node (Just 'c') []])
-- Just 'a'
