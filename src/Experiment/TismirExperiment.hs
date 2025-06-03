{-# LANGUAGE TupleSections #-}

module Experiment.TismirExperiment (
    -- * (What are the patterns?) visualizing pattern as tree fragment
    patternAsComputation,
    patternSizeExpanded,


    -- * (Pattern dependencies)
    patternDependents,
    directDependents,
    directDependencies,
    patternImpact,

    -- * (Where are the patterns?) pattern usage

    -- patternGlobalFreq,
    patternFreqInCorpus,
    patternOccuranceG,
    highlightPatternInCorpus,
    drawOccuranceInCorpus,
    patternDepthInCorpus,
    markPatternIdInCorpus,
    getRuleDepth,
    SizeFreqDistribution (..),
    OccuranceHeatMap (..),

    -- * helper function for reporting pattern together with a feature of interest
    report,
) where

import Compression.AttributedSLFP (SLFPAttributed)
import qualified Compression.AttributedSLFP as Attributed
import Compression.SLFP
import Control.Arrow
import Control.Monad.Fix (MonadFix (mfix))
import Data.Functor.Foldable
import Data.Functor.Foldable.TH
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe
import Data.Set (Set)
import qualified Data.Set as Set

import GHC.Generics

import Compression.TreeUtils (sizeTree)
import Data.Aeson
import Data.Function
import Data.Functor.Base
import Data.List
import Data.Ord
import Data.Tree
import Diagrams.Backend.SVG
import Diagrams.Prelude
import Grammar.JazzHarmony.JazzGrammar
import Prettyprinter (pretty)
import Visualization.BackEnd (BackEnd)
import Visualization.Text
import Visualization.Tree (treeDiagram)

type MetaID = String
type PatternID = String

-- What are the patterns? --

patternAsComputation :: (_) => SLFP r k -> PatternID -> Tree (Abstraction r)
patternAsComputation slfp patId =
    
    fixedPoint
            ( deCompressTree
                (globalPatterns slfp Map.!)
                (globalMetas slfp Map.!)
                (arities slfp Map.!)
            )
        $ startingTree
  where
    startingTree = Node (Var patId) 
        $ replicate (inferArity 
            (globalPatterns slfp Map.!) (globalMetas slfp Map.!) (arities slfp Map.!) (Var patId)) 
        $ Node Hole []

patternSizeExpanded :: (_) => SLFP r k -> PatternID -> Int
patternSizeExpanded slfp = sizeTree . patternAsComputation slfp

-- Where are the patterns? --

containsPattern ::
    (_) =>
    -- | global dependents (containing indirect ones)
    (PatternID -> Set PatternID) ->
    PatternID ->
    SLTP r ->
    Bool
containsPattern dependents p sltp =
    if null (dependents p)
        then any (matchAbstraction p) . compressedTree $ sltp
        else any (\p' -> containsPattern dependents p' sltp) (dependents p)

extractVar :: Abstraction a -> Maybe String
extractVar (Var x) = Just x
extractVar _ = Nothing

matchAbstraction :: PatternID -> Abstraction r -> Bool
matchAbstraction p = \case
    Var s -> p == s
    With x (mID, xs) -> any (matchAbstraction p) (x : xs)
    Constant _ -> False

isPattern :: PatternID -> Abstraction r -> Bool
isPattern p = \case
    Var s -> p == s
    _ -> False

matchPattern :: PatternID -> Pattern (Abstraction r) -> Bool
matchPattern p (Comp i x y) = any (matchAbstraction p) [x, y]

reachables :: (Ord a) => (a -> Set a) -> a -> Set a
reachables f initial = fixedPoint' Set.empty (f initial)
  where
    fixedPoint' seen current
        | Set.null newElements = current
        | otherwise = fixedPoint' newSeen (Set.union current newResults)
      where
        newElements = current `Set.difference` seen
        newResults = Set.unions (map f (Set.toList newElements))
        newSeen = Set.union seen current

-- visualized occurance in piece --

highlightDecompressed :: (_) => SLFP a k -> PatternID -> SLFPAttributed Bool a k
highlightDecompressed slfp p =
    fixedPoint
        (Attributed.deCompressGAttributed False updateAttr . Attributed.highlightNodeG (isPattern p))
        $ Attributed.toSLFPAttributed (isPattern p) slfp
  where
    updateAttr b pID i x = b

highlightPatternInCorpus ::
    (_) =>
    SLFP a k ->
    PatternID ->
    Map k (Tree (Bool, Abstraction a))
highlightPatternInCorpus slfp p =
    highlightDecompressed slfp p
        & Attributed.sltpsAttributed
        & fmap Attributed.compressedTree
        & Map.filter (any fst)

markPatternIdDecompressed ::
    (_) =>
    SLFP a k ->
    SLFPAttributed [PatternID] a k
markPatternIdDecompressed slfp =
    fixedPoint
        (Attributed.deCompressGAttributed [] updateAttr)
        $ Attributed.toSLFPAttributed initPatternList slfp
  where
    updateAttr b pID i x = b ++ [pID]
    initPatternList (Var x) = [x]
    initPatternList _ = []

markPatternIdInCorpus ::
    (_) =>
    SLFP a k ->
    Map k (Tree ([PatternID], Abstraction a))
markPatternIdInCorpus =
    fmap Attributed.compressedTree
        . Attributed.sltpsAttributed
        . markPatternIdDecompressed

patternFreqInCorpus :: (_) => SLFP a k -> Map PatternID Int
patternFreqInCorpus slfp =
    Map.filterWithKey
        (\k _ -> k `Map.member` globalPatterns slfp)
        . Map.unionsWith (+)
        . fmap (getPatternCount . fmap fst)
        . Map.elems
        . markPatternIdInCorpus
        $ slfp

getPatternCount :: (_) => Tree [k] -> Map k Int
getPatternCount =
    foldl'
        (foldr (\pId -> Map.insertWith (+) pId 1))
        mempty

-- >>> getPatternCount (Node ["a","b"] [Node [] [], Node ["a"] [Node ["d"] []]])
-- fromList [("a",2),("b",1),("d",1)]

getRuleDepth :: (_) => Tree r -> Map r [Double]
getRuleDepth t =
    Map.unionsWith
        (++)
        ((\(r, d) -> Map.singleton r [d]) <$> withDepth t)

-- >>>  getRuleDepth (Node "a" [Node "g" [], Node "a" [Node "d" []]])
-- fromList [("a",[0.0,0.5]),("d",[1.0]),("g",[0.5])]

patternDepthInCorpus ::
    (_) =>
    SLFP a k ->
    Map PatternID [Double]
patternDepthInCorpus slfp =
    Map.filterWithKey
        (\k _ -> k `Map.member` globalPatterns slfp)
        . Map.unionsWith (++)
        . fmap (getPatternDepth . fmap fst)
        . Map.elems
        . markPatternIdInCorpus
        $ slfp

getPatternDepth :: (_) => Tree [k] -> Map k [Double]
getPatternDepth t =
    Map.fromListWith (++) $
        foldMap
            (\(ids, d) -> (,[d]) <$> ids)
            (withDepth t) -- treeWithDepth t

withDepth :: Tree a -> Tree (a, Double)
withDepth t = go 0 t
  where
    go n (Node a ts) = Node (a, n / total) $ go (n + 1) <$> ts
    total = fromIntegral $ depth t

-- >>> getPatternDepth (Node ["a","b"] [Node [] [], Node ["a"] [Node ["d"] []]])
-- fromList [("a",[0.5,0.0]),("b",[0.0]),("d",[1.0])]

depth :: Tree a -> Int
depth = cata $ \case
    NodeF _ [] -> 0
    NodeF _ xs -> 1 + maximum xs

-- | the set of patterns that are  dependent of the pattern
patternDependents :: SLFP r k -> PatternID -> Set PatternID
patternDependents slfp = reachables (directDependents slfp)

directDependents :: SLFP r k -> PatternID -> Set PatternID
directDependents slfp p =
    Map.keysSet $
        Map.filter
            (matchPattern p)
            (globalPatterns slfp)

directDependencies :: (_) => SLFP r k -> PatternID -> Set PatternID
directDependencies slfp p = case globalPatterns slfp Map.! p of
    Comp i x y -> foldMap getVariables [x, y]

-- | the set of pieces that contains the pattern
patternOccuranceG ::
    (_) =>
    SLFP r k ->
    PatternID ->
    Set k
patternOccuranceG slfp p =
    Map.keysSet
        . Map.filter (containsPattern (patternDependents slfp) p)
        . sltps
        $ slfp

metaOccurance :: SLFP r k -> MetaID -> [k]
metaOccurance = undefined

allPatterns :: SLFP a b -> [PatternID]
allPatterns = Map.keys . globalPatterns

-- | the number of pieces that contains the pattern
patternGlobalFreq :: SLFP r k -> PatternID -> Int
patternGlobalFreq slfp = Set.size . patternOccuranceG slfp

newtype OccuranceHeatMap a k = OccuranceHeatMap [(a, k)]

newtype SizeFreqDistribution a
    = SizeFreqDistribution
        [(a, Int, Int)]

-- What is the dependency among patterns? --

allDependents :: SLFP r k -> Map PatternID (Set PatternID)
allDependents slfp = Map.fromList $ (id &&& patternDependents slfp) <$> allPatterns slfp

data KeyValuePair k v = KeyValuePair {name :: k, value :: v}
    deriving (Generic)

instance (ToJSON k, ToJSON v) => ToJSON (KeyValuePair k v)
instance (FromJSON k, FromJSON v) => FromJSON (KeyValuePair k v)

-- helper function for reporting --
report :: (SLFP a b -> PatternID -> c) -> SLFP a b -> [KeyValuePair PatternID c]
report f slfp = uncurry KeyValuePair . (id &&& f slfp) <$> allPatterns slfp

-- visualization --

drawColoredTree :: (a -> Diagram BackEnd) -> Tree (Colour Double, a) -> Diagram BackEnd
drawColoredTree f = treeDiagram drawNode
  where
    drawNode (c, a) = f a <> (circle 2 # fc c # lw none)

type PieceID = String

drawOccuranceInCorpus :: PatternID -> Map PieceID (Tree (Bool, Abstraction RuleNames)) -> Diagram BackEnd
drawOccuranceInCorpus p d =
    hsep
        1
        [ drawText p
        , vsep 1 (uncurry drawOccuranceInPiece <$> Map.toList d)
        ]

drawOccuranceInPiece :: PieceID -> Tree (Bool, Abstraction RuleNames) -> Diagram BackEnd
drawOccuranceInPiece pieceId t =
    vsep
        1
        [ drawText pieceId
        , drawColoredTree (drawText . show . pretty) $
            fmap highlightedToColored t
        ]
highlightedToColored (True, a) = (brown, a)
highlightedToColored (False, a) = (white, a)

patternImpact ::
    (PatternID -> Int) ->
    (PatternID -> [PatternID]) ->
    PatternID ->
    Double
patternImpact freq dependents pId =
    sumOverRank $
        fromIntegral . freq <$> dependents pId

sumOverRank :: [Double] -> Double
sumOverRank xs = sum $ zipWith (*) [1 ..] $ sortBy (comparing Down) xs

-- >>> sumOverRank [2,5,1]
-- 12.0
