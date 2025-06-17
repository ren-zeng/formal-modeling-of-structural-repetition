{-# LANGUAGE TupleSections #-}

module Compression.AttributedSLFP (
    toSLFPAttributed,
    deCompressGAttributed,
    SLTPAttributed (..),
    SLFPAttributed (..),
    highlightNodeG,
) where

import Compression.Meta
import Compression.SLFP hiding (arities, compressedTree, globalMetas, globalPatterns, metas, patterns)
import qualified Compression.SLFP as SLFP
import Compression.TreeUtils
import Control.Arrow
import Data.Bool (bool)
import Data.Functor.Base (TreeF (..))
import Data.Functor.Foldable
import Data.List.Split
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Tree
import Control.Monad
import Data.Foldable

deCompressTreeAttributed ::
    (Ord a, _) =>
    b -> -- default value for the attribute
    (b -> String -> Int -> Abstraction a -> b) ->
    (String -> Pattern (Abstraction a)) ->
    (String -> Meta) ->
    (Abstraction a -> Int) ->
    Tree (b, Abstraction a) ->
    Tree (b, Abstraction a)
deCompressTreeAttributed b f dP dM dA t@(Node r ts) = case snd r of
    (Constant _) -> Node r (deCompressTreeAttributed b f dP dM dA <$> ts)
    s@(Var x) ->
        pSubstituteAllAttributed
            b
            (inferArity dP dM dA y)
            (x, dP x)
            f
            t
            (findSlots ((== s) . snd) t)
      where
        (Comp _ _ y) = dP x
    s@(x `With` (m, as)) ->
        rSubstituteAllAttributed
            b
            (s, groupings, meta)
            t
            (findSlots ((== s) . snd) t)
      where
        groupings = inferArity dP dM dA <$> useMeta meta x as
        meta = dM m
    Hole -> t

pSubstituteAllAttributed ::
    (_) =>
    b ->
    Int ->
    (String, Pattern (Abstraction a)) ->
    (b -> String -> Int -> Abstraction a -> b) ->
    Tree (b, Abstraction a) ->
    [Location] ->
    Tree (b, Abstraction a)
pSubstituteAllAttributed defB n e f =
    applyPostOrder (pSubstitutionTopAttributed defB n e f)

rSubstituteAllAttributed ::
    (_) =>
    b ->
    (Abstraction a, [Int], Meta) ->
    Tree (b, Abstraction a) ->
    [Location] ->
    Tree (b, Abstraction a)
rSubstituteAllAttributed b e = applyPostOrder (rSubstitutionTopAttributed b e)

pSubstitutionTopAttributed ::
    (_) =>
    b ->
    Int ->
    (String, Pattern (Abstraction a)) ->
    (b -> String -> Int -> Abstraction a -> b) ->
    Tree (b, Abstraction a) ->
    Tree (b, Abstraction a)
pSubstitutionTopAttributed defB n (patID, pat) f (Node (b, _) ts) =
    case pat of
        Comp i r y ->
            Node (updatedAttr, r) $
                take (i - 1) ts
                    ++ [Node (initAttr y, y) (take n . drop (i - 1) $ ts)]
                    ++ drop (i + n - 1) ts
          where
            -- only the top g's atttribute in @g oi f@ updated (for the task of locating the pattern)
            updatedAttr = f b patID i r
        TreePattern t -> fillHoles ((==Hole) . snd) 
            t'{rootLabel = (f b patID undefined r,r) } 
            ts
            where 
                t' = (initAttr &&& id) <$> t
                (b,r) = rootLabel t'
  where
    initAttr = const defB





rSubstitutionTopAttributed ::
    (_) =>
    b ->
    (Abstraction a, [Int], Meta) ->
    Tree (b, Abstraction a) ->
    Tree (b, Abstraction a)
rSubstitutionTopAttributed defB (rt `With` (_, abs), ns, m) (Node (b, _) ts) =
    Node (b, rt) ts'
  where
    ts' =
        zipWith
            Node
            ((defB,) <$> useMeta m rt abs)
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
highlightNode :: (a -> Bool) -> Tree (Bool, a) -> Tree (Bool, a)
highlightNode f = fmap (\(b, x) -> if f x then (True, x) else (b, x))

highlightNodeG ::
    (Abstraction a -> Bool) ->
    SLFPAttributed Bool a k ->
    SLFPAttributed Bool a k
highlightNodeG f =
    toGlobal
        ( \sltp ->
            sltp
                { compressedTree = highlightNode f $ compressedTree sltp
                }
        )

toGlobal ::
    (SLTPAttributed b a -> SLTPAttributed b a) ->
    SLFPAttributed b a k ->
    SLFPAttributed b a k
toGlobal f slfp = slfp{sltpsAttributed = f <$> sltpsAttributed slfp}

toSLFPAttributed :: (Abstraction a -> b) -> SLFP a k -> SLFPAttributed b a k
toSLFPAttributed f slfp =
    SLFPAttributed
        { sltpsAttributed = toSLTPAttributed f <$> SLFP.sltps slfp
        , globalPatterns = SLFP.globalPatterns slfp
        , globalMetas = SLFP.globalMetas slfp
        , arities = SLFP.arities slfp
        }

deCompressGAttributed ::
    (_) =>
    b ->
    (b -> String -> Int -> Abstraction a -> b) ->
    SLFPAttributed b a k ->
    SLFPAttributed b a k
deCompressGAttributed defB f g@(SLFPAttributed dE dpG drG dA) = SLFPAttributed dE' dpG drG dA
  where
    dE' = deCompressLAttributed defB f dpG drG dA <$> dE

deCompressLAttributed ::
    (Ord a, _) =>
    b ->
    (b -> String -> Int -> Abstraction a -> b) ->
    Map String (Pattern (Abstraction a)) ->
    Map String Meta ->
    Map (Abstraction a) Int ->
    SLTPAttributed b a ->
    SLTPAttributed b a
deCompressLAttributed defB f dpG drG dA (SLTPAttributed t dP dM) = SLTPAttributed t' dP dM
  where
    t' = deCompressTreeAttributed defB f 
        ((dpG <> dP) `debugLookup`) 
        ((drG <> dM) `debugLookup`) 
        (dA `debugLookup`) 
        t