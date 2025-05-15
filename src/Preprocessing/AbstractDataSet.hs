{-# LANGUAGE TypeData #-}
module Preprocessing.AbstractDataSet where

import Core.ParseTree
import Core.SymbolTree

import qualified Grammar.JazzHarmony.JazzGrammar as HarmonyGrammar

import qualified Grammar.JazzHarmony.MusicTheory as MusicTheory
import  qualified Preprocessing.JazzHarmony.TreeBankLoader as JHTB



rootSymbol :: SymbolTree nt t -> Symbol nt t
rootSymbol (TLeaf x) = T x
rootSymbol (NTNode x _) = NT x

inferRuleTree ::
    (nt -> [Symbol nt t] -> Maybe r) ->
    SymbolTree nt t ->
    Maybe (ParseTree r nt t)
inferRuleTree inferRule = \case
    NTNode nt ts -> do
        r <- inferRule nt (Core.SymbolTree.rootSymbol <$> ts)
        ts' <- mapM (inferRuleTree inferRule) ts
        return $ ParseTree nt r ts'
    TLeaf t -> return $ Leaf t

titleWithParseTree :: 
    (a -> Maybe (ParseTree r nt t))
    -> (a -> k)
    -> a
    -> Maybe (k,ParseTree r nt t)
titleWithParseTree getParseT getName p = do 
    t <- getParseT p
    return (getName p,t)


class ParseTreeInferable a where
    type Rule a
    type NT a
    type T a
    type Piece a




data JazzHarmonyTreeBank 
data RockRhythmTreeBank 
data ClassicalRhythmTreeBank

instance ParseTreeInferable JazzHarmonyTreeBank where 
    type Rule JazzHarmonyTreeBank = HarmonyGrammar.RuleNames
    type NT JazzHarmonyTreeBank = MusicTheory.ChordLabel
    type T JazzHarmonyTreeBank = MusicTheory.ChordLabel
    type Piece JazzHarmonyTreeBank = JHTB.Piece
