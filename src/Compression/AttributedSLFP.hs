{-# LANGUAGE TupleSections #-}

module Compression.AttributedSLFP (
    toSLFPAttributed,
    deCompressGAttributed,
    SLTPAttributed (..),
    SLFPAttributed (..),
    highlightNodeG
) where

import Compression.Meta
import Compression.SLFP hiding (arities, compressedTree, globalMetas, globalPatterns, metas, patterns)
import qualified Compression.SLFP as SLFP
import Compression.TreeUtils
import Control.Arrow
import Data.List.Split
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Tree
import Data.Bool (bool)

deCompressTreeAttributed ::
    (Ord a) =>
    (String -> Pattern (Abstraction a)) ->
    (String -> Meta) ->
    (Abstraction a -> Int) ->
    Tree (b, Abstraction a) ->
    Tree (b, Abstraction a)
deCompressTreeAttributed dP dM dA t@(Node r ts) = case snd r of
    (Constant _) -> Node r (deCompressTreeAttributed dP dM dA <$> ts)
    s@(Var x) ->
        pSubstituteAllAttributed
            (inferArity dP dM dA y)
            (dP x)
            t
            (findSlots ((== s) . snd) t)
      where
        (Comp _ _ y) = dP x
    s@(x `With` (m, as)) ->
        rSubstituteAllAttributed
            (s, groupings, meta)
            t
            (findSlots ((== s) . snd) t)
      where
        groupings = inferArity dP dM dA <$> useMeta meta x as
        meta = dM m

pSubstituteAllAttributed :: Int -> Pattern a -> Tree (b, a) -> [Location] -> Tree (b, a)
pSubstituteAllAttributed n e = applyPostOrder (pSubstitutionTopAttributed n e)

rSubstituteAllAttributed :: (Abstraction a, [Int], Meta) -> Tree (b, Abstraction a) -> [Location] -> Tree (b, Abstraction a)
rSubstituteAllAttributed e = applyPostOrder (rSubstitutionTopAttributed e)

pSubstitutionTopAttributed ::
    Int ->
    Pattern a ->
    Tree (b, a) ->
    Tree (b, a)
pSubstitutionTopAttributed n (Comp i r y) (Node (b, _) ts) =
    Node (b, r) $
        take (i - 1) ts ++ [Node (b, y) (take n . drop (i - 1) $ ts)] ++ drop (i + n - 1) ts

rSubstitutionTopAttributed ::
    (Abstraction a, [Int], Meta) ->
    Tree (b, Abstraction a) ->
    Tree (b, Abstraction a)
rSubstitutionTopAttributed (rt `With` (_, abs), ns, m) (Node (b, _) ts) =
    Node (b, rt) ts'
  where
    ts' =
        zipWith
            Node
            ((b,) <$> useMeta m rt abs)
            (splitPlacesBlanks ns ts)

data SLTPAttributed b a = SLTPAttributed
    { compressedTree :: Tree (b, Abstraction a)
    , patterns :: Map String (Pattern (Abstraction a))
    , metas :: Map String Meta
    }
    deriving (Show, Eq)

toSLTPAttributed :: (Abstraction a -> b) -> SLTP a -> SLTPAttributed b a
toSLTPAttributed f sltp =
    SLTPAttributed
        { compressedTree = (f &&& id) <$> SLFP.compressedTree sltp
        , patterns = SLFP.patterns sltp
        , metas = SLFP.metas sltp
        }

data SLFPAttributed b a k = SLFPAttributed
    { sltpsAttributed :: Map k (SLTPAttributed b a)
    , globalPatterns :: Map String (Pattern (Abstraction a))
    , globalMetas :: Map String Meta
    , arities :: Map (Abstraction a) Int
    }
    deriving (Show, Eq)

-- | only additive, don't unhighlight node
highlightNode :: (a -> Bool) -> Tree (Bool,a) -> Tree (Bool,a)
highlightNode f = fmap (\(b,x) -> if f x then (True,x) else (b,x))

highlightNodeG ::
    (Abstraction a -> Bool) ->
    SLFPAttributed Bool a k ->
    SLFPAttributed Bool a k
highlightNodeG f = toGlobal (\sltp -> sltp 
    {compressedTree = highlightNode f $ compressedTree sltp})

toGlobal :: (SLTPAttributed b a -> SLTPAttributed b a) 
    -> SLFPAttributed b a k -> SLFPAttributed b a k
toGlobal f slfp = slfp{sltpsAttributed = f <$> sltpsAttributed slfp}


toSLFPAttributed :: (Abstraction a -> b) -> SLFP a k -> SLFPAttributed b a k
toSLFPAttributed f slfp =
    SLFPAttributed
        { sltpsAttributed = toSLTPAttributed f <$> SLFP.sltps slfp
        , globalPatterns = SLFP.globalPatterns slfp
        , globalMetas = SLFP.globalMetas slfp
        , arities = SLFP.arities slfp
        }

deCompressGAttributed :: (_) => SLFPAttributed b a k -> SLFPAttributed b a k
deCompressGAttributed g@(SLFPAttributed dE dpG drG dA) = SLFPAttributed dE' dpG drG dA
  where
    dE' = deCompressLAttributed dpG drG dA <$> dE

deCompressLAttributed ::
    (Ord a) =>
    Map String (Pattern (Abstraction a)) ->
    Map String Meta ->
    Map (Abstraction a) Int ->
    SLTPAttributed b a ->
    SLTPAttributed b a
deCompressLAttributed dpG drG dA (SLTPAttributed t dP dM) = SLTPAttributed t' dP dM
  where
    t' = deCompressTreeAttributed ((dpG <> dP) Map.!) ((drG <> dM) Map.!) (dA Map.!) t