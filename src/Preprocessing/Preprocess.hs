{-# LANGUAGE TypeData #-}

module Preprocessing.Preprocess where

import Core.ParseTree
import Core.SymbolTree

import Control.Concurrent.Async (mapConcurrently_)
import Data.Aeson
import Data.Functor.Foldable
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe
import Data.Tree
import Diagrams hiding (Result)
import Diagrams.Backend.SVG
import Diagrams.Prelude hiding (Result)
import GHC.Generics
import qualified Grammar.JazzHarmony.MusicTheory as MusicTheory
import qualified Preprocessing.JazzHarmony.TreeBankLoader as JHTB
import Prettyprinter
import System.Directory
import Text.Printf
import VisualHTML.SvgToPdf
import Visualization.ParseTree
import Visualization.Text

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

plotProofTree :: (_) => FilePath -> k -> ParseTree r nt t -> IO ()
plotProofTree outFolder name t =
    renderSVG
        (printf "%s/%s.svg" outFolder $ show $ pretty name)
        (mkWidth 1000)
        ( Diagrams.bg white $
            drawParseTree
                (frame 0.25 . drawText . show . pretty)
                (drawSymbol (drawText . show . pretty) (drawText . show . pretty))
                t
        )

drawSymbol :: (nt -> Diagram B) -> (t -> Diagram B) -> Symbol nt t -> Diagram B
drawSymbol drawNT drawT = \case
    T x -> drawT x # opacity 0.2
    NT x -> drawNT x

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

getAllRules :: ParseTree r nt t -> [r]
getAllRules = cata $ \case
    LeafF _ -> []
    ParseTreeF _ r rss -> r : concat rss

histogram :: (Ord a) => [a] -> Map a Int
histogram = foldr (\x acc -> Map.insertWith (+) x 1 acc) Map.empty

data Count a = Count {feature :: a, frequency :: Int}
    deriving (Generic, Show, Functor)
instance (ToJSON a) => ToJSON (Count a)
instance (FromJSON a) => FromJSON (Count a)

toCounts :: Map a Int -> [Count a]
toCounts = fmap (uncurry Count) . Map.toList

data Categorized a b = Categorized
    { content :: a
    , category :: b
    }
    deriving (Generic, Show, Functor, Eq, Ord)
instance (ToJSON a, ToJSON b) => ToJSON (Categorized a b)
instance (FromJSON a, FromJSON b) => FromJSON (Categorized a b)

preprocess :: (_) => IO ([e],[a]) -> (r -> r') -> (a -> ParseTree (Maybe r) nt t) -> (a -> k) -> FilePath -> IO ()
preprocess load ruleCategory getParseTree getPieceName outDir = do
    (errors,ps) <- load
    let xs = fmap (titleWithParseTree getParseTree getPieceName) ps
    encodeFile (outDir <> "/TreeBankLoadingErrors.json") errors
    encodeFile (outDir <> "/ParseTrees.json") xs
    encodeFile (outDir <> "/ParseTreeReport.json") $
        ParseTreeReport
            { allRuleInfered = length $ filter (withoutCatchAll . snd) xs
            , containsPlaceHolderRule =
                length $
                    filter (not . withoutCatchAll . snd) xs
            }

    encodeFile (outDir <> "/RuleDistribution.json") $
        toCounts . Map.mapKeys (show . pretty) . histogram $
            foldMap (getAllRules . snd) xs

    encodeFile (outDir <> "/RuleDistributionCategory.json")
        $ toCounts
            . Map.mapKeys
                (fmap $ \x -> Categorized (show $ pretty x) (ruleCategory x))
            . histogram
        $ foldMap (getAllRules . snd) xs

    let parseTreeDir = outDir <> "/InferedParseTrees"
    let svgFolder = parseTreeDir <> "/svg"
    let pdfFolder = parseTreeDir <> "/pdf"

    emptyDirectory parseTreeDir
    createDirectory svgFolder
    mapConcurrently_ (uncurry $ plotProofTree (outDir <> "/InferedParseTrees/svg")) xs

    createDirectory pdfFolder
    svgFiles <- listDirectory svgFolder
    convertSvgsToPdf ((\x -> svgFolder <> "/" <> x) <$> svgFiles) pdfFolder

instance (ToJSONKey a, ToJSON a) => ToJSONKey (Maybe a)

emptyDirectory :: FilePath -> IO ()
emptyDirectory path = do
    createDirectoryIfMissing False path
    removeDirectoryRecursive path
    createDirectory path