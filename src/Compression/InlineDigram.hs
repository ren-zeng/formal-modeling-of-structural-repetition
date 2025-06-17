module Compression.InlineDigram where

import Compression.Meta
import Compression.SLFP
import Data.Functor.Base
import Data.Functor.Foldable
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Tree
import Debug.Trace
import Safe (readMay)

type Arity = Int

inlineDigramTop ::
  (_) =>
  (Abstraction a -> Int) ->
  (String -> Pattern (Abstraction a)) ->
  Tree (Abstraction a) ->
  Tree (Abstraction a)
inlineDigramTop getArity getPattern t = case t of
  Node (Constant _) _ -> t
  Node (Var x) _ ->
    let inlined = pSubstitutionTop (getArity (Var x)) (getPattern x) t
     in if effectiveDepth inlined > 1
          then
            t
          else inlineDigramTop getArity getPattern inlined
  Node (With{}) _ -> t
  Node Hole _ -> t

-- | the effective depth of a tree is the maximum depth of any node that is not a hole. It represents how many layers of computation are composed together.
effectiveDepth :: Tree (Abstraction a) -> Int
effectiveDepth = cata $ \case
  NodeF Hole [] -> 0
  NodeF _ [] -> 1
  NodeF _ ns -> 1 + maximum ns

-- >>> effectiveDepth (Node (Constant "a") [Node (Constant "a") [Node Hole []]])
-- 2

inlineRRewriteTop ::
  (_) =>
  (Meta -> String) ->
  Tree (Abstraction a) ->
  Tree (Abstraction a)
inlineRRewriteTop mtID t
  | not $ any ((== Hole) . rootLabel) $ subForest t =
      let
        mt = inferMeta (rootLabel t) (rootLabel <$> subForest t)
       in
        if nonTrivial mt 
          then  rRewriteTop (mtID mt, mt) t
        else t
  | otherwise = t

testMTID :: Meta -> String
testMTID = show

-- >>> inlineRRewriteTop testMTID (Node (Constant "a") [Node (Constant "a") [Node Hole [],Node Hole [],Node Hole []], Node (Constant "b") [],Node (Constant "b") []])
-- Node {rootLabel = Constant "a" `With` ("[\9733,_,1]",[Constant "b"]), subForest = [Node {rootLabel = Hole, subForest = []},Node {rootLabel = Hole, subForest = []},Node {rootLabel = Hole, subForest = []}]}

-- | Inline digram patterns in a tree to produce tree with effective depth = 1. Apply rRewrite when possible.
inlineDigramWithMeta ::
  (Eq a, _) =>
  (Meta -> String) ->
  (Abstraction a -> Int) ->
  (String -> Pattern (Abstraction a)) ->
  Tree (Abstraction a) ->
  Tree (Abstraction a)
inlineDigramWithMeta mtID getArity getPattern =
  inlineRRewriteTop mtID . inlineDigramTop getArity getPattern

metaStageCompress ::
  (_) =>
  (Meta -> String) ->
  SLFP a b ->
  SLFP a b
metaStageCompress mtID slfp =
  slfp
    { globalPatterns = dP'
    , globalMetas = Map.union dM $ Map.fromList $ do
        m <- foldMap metaInPattern $ Map.elems dP'

        return (show m, m)
    }
 where
  dP' = TreePattern . inlineDigramWithMeta mtID getArity (debugLookup dP) . digramAsTree getArity <$> dP
  dP = globalPatterns slfp
  dM = globalMetas slfp
  getArity = inferArity (debugLookup dP) (debugLookup dM) (debugLookup $ arities slfp)

metaInPattern :: _ => Pattern (Abstraction a) -> [Meta]
metaInPattern = \case
  Comp i (_ `With` (s, _)) y -> pure $ readMeta s
  TreePattern (Node (_ `With` (s, _)) _) -> pure $ readMeta s
  p -> []
 where
  readMeta x = case readMay x of
    Just m -> m
    Nothing -> error $ "can not read meta: " <> show x

digramAsTree ::
  (Abstraction a -> Int) ->
  Pattern (Abstraction a) ->
  Tree (Abstraction a)
digramAsTree getArity (Comp i g f) =
  Node g $
    replicate (i - 1) hole
      ++ [Node f $ replicate (getArity f) hole]
      ++ replicate (getArity g - i) hole
 where
  hole = Node Hole []
