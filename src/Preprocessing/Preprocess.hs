{-# LANGUAGE TypeData #-}

module Preprocessing.Preprocess where

import Core.ParseTree
import Core.SymbolTree

import Data.Maybe
import Data.Tree
import Diagrams hiding (Result)
import Diagrams.Backend.SVG
import Diagrams.Prelude hiding (Result)
import qualified Grammar.JazzHarmony.MusicTheory as MusicTheory
import qualified Preprocessing.JazzHarmony.TreeBankLoader as JHTB
import Prettyprinter
import Text.Printf
import Visualization.ParseTree
import Visualization.Text
import GHC.Generics
import Data.Aeson
import System.Directory

rootSymbol :: SymbolTree nt t -> Symbol nt t
rootSymbol (TLeaf x) = T x
rootSymbol (NTNode x _) = NT x

inferRuleTree ::
    (nt -> [Symbol nt t] -> Maybe r) ->
    SymbolTree nt t ->
    ParseTree (Maybe r) nt t
inferRuleTree inferRule = \case
    NTNode nt ts ->
        ParseTree
            nt
            (inferRule nt (Core.SymbolTree.rootSymbol <$> ts))
            (fmap (inferRuleTree inferRule) ts)
    TLeaf t -> Leaf t

withoutCatchAll :: (_) => ParseTree (Maybe r) nt t -> Bool
withoutCatchAll t = do
    case parseTreeToRuleTree t of
        Nothing -> False
        Just ruleTree -> Nothing `notElem` ruleTree

titleWithParseTree ::
    (_) =>
    (a -> ParseTree (Maybe r) nt t) ->
    (a -> k) ->
    a ->
    (k, ParseTree (Maybe r) nt t)
titleWithParseTree getParseT getName x = (k, t)
  where
    t = getParseT x
    k =
        if withoutCatchAll t
            then "(Valid)" <> getName x
            else "(Invalid)" <> getName x

parseTreeToRuleTree ::
    ParseTree r nt t ->
    Maybe (Tree r)
parseTreeToRuleTree = \case
    Leaf _ -> Nothing
    ParseTree _ r ts -> return $ Node r $ mapMaybe parseTreeToRuleTree ts

plotProofTree :: (_) => FilePath ->k -> ParseTree r nt t -> IO ()
plotProofTree outFolder name  t = renderSVG
        (printf "%s/%s.svg" outFolder $ show $ pretty name)
        (mkWidth 1000)
        (Diagrams.bg white $ drawParseTree (frame 0.25 . drawText . show . pretty) (drawText . show . pretty) t)

data ParseTreeReport = ParseTreeReport
    { allRuleInfered :: Int
    , containsPlaceHolderRule :: Int
    }
    deriving (Generic, Show)
instance ToJSON ParseTreeReport
instance FromJSON ParseTreeReport

toMaybe :: Result a -> Maybe a
toMaybe (Success x) = Just x
toMaybe _ = Nothing

isSuccess :: Result a -> Bool
isSuccess (Success x) = True
isSuccess _ = False

preprocess :: _ => IO [a] -> (a -> ParseTree (Maybe r) nt t) -> (a -> k) -> FilePath -> IO ()
preprocess load getParseTree getPieceName outDir = do
    ps <- load 
    let xs = fmap (titleWithParseTree getParseTree getPieceName) ps
    encodeFile (outDir <> "/ParseTrees.json") xs
    encodeFile (outDir <> "/ParseTreeReport.json") $
        ParseTreeReport
            { allRuleInfered = length $ filter (withoutCatchAll . snd) xs
            , containsPlaceHolderRule =
                length $
                    filter (not . withoutCatchAll . snd) xs
            }
    -- removeDirectory $ outDir <> "/InferedParseTrees"
    createDirectory $ outDir <> "/InferedParseTrees"
    mapM_ (uncurry $ plotProofTree (outDir <> "/InferedParseTrees")) xs

