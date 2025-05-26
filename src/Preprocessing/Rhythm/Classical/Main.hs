module Preprocessing.Rhythm.Classical.Main where

import Control.Arrow
import Control.Monad

import qualified Diagrams as Diagram
import Diagrams.Backend.SVG
import Diagrams.Prelude
import Prettyprinter

-- import System.Directory.Extra (createDirectory, createDirectoryIfMissing)

import Core.ParseTree

import Control.Applicative (Alternative (empty))
import Data.Aeson (FromJSON, ToJSON, ToJSONKey, encodeFile)
import Data.Maybe (mapMaybe)
import GHC.Generics

import Grammar.Rhythm.RhythmGrammar
import Preprocessing.Preprocess (ParseTreeReport (..), inferRuleTree, plotProofTree, preprocess, titleWithParseTree, withoutCatchAll)
import Preprocessing.Rhythm.Classical.TreeBankLoader
import RIO.Directory
import Text.Printf
import Visualization.ProofTree
import Visualization.SymbolTree
import Visualization.Text

plotDecodedJSON :: FilePath -> Piece -> IO ()
plotDecodedJSON path p =
    renderSVG
        (path <> printf "/DecodedParseTree/%s.svg" (pieceId p))
        (Diagram.mkSizeSpec2D (Just 1000) (Just 1000))
        (drawRhythmTree $ pieceTree p)

plotCorrection :: FilePath -> Piece -> IO ()
plotCorrection path p = do
    let orginSymbolTree = (toSymbolTree . pieceTree) p
    let correctedSymbolTree = correctSymbolTree orginSymbolTree
    renderSVG
        (path <> printf "/CorrectionParseTree/%s.svg" (pieceId p))
        (Diagram.mkSizeSpec2D (Just 1000) (Just 1000))
        (Diagram.bg white . Diagram.vsep 1 $ drawSymbolTree (drawText . show . pretty) <$> [orginSymbolTree, correctedSymbolTree])

-- plotProofTree :: FilePath -> Piece -> IO ()
-- plotProofTree path p =
--     renderSVG
--         (path <> printf "/ProofTrees/%s.svg" (pieceId p))
--         (Diagram.mkSizeSpec2D (Just 1000) (Just 1000))
--         (proofDiagram $ parseTreeToProofTree parseTree)
--   where
--     parseTree = getParseTree p
--     proofDiagram t =
--         drawProofTree
--             (drawText . show . pretty)
--             (drawText . show . pretty)
--             t
--             # Diagram.bg white

reportPreprocessing :: FilePath -> FilePath -> IO ()
reportPreprocessing datasetPath outPath = do
    pieces <- load datasetPath
    let folderNames = ["DecodedParseTree", "CorrectionParseTree", "ProofTrees"]
    forM_ folderNames (\x -> createDirectoryIfMissing False $ outPath <> "/" <> x)
    mapM_ (plotDecodedJSON outPath) pieces
    mapM_ (plotCorrection outPath) pieces

-- mapM_ (plotProofTree outPath) pieces

preprocessRhythmCorpus :: FilePath -> IO ()
preprocessRhythmCorpus folder =
    reportPreprocessing
        (folder <> "/RhythmTreeBank.json")
        folder

dataSetFolder :: FilePath
dataSetFolder = "Experiment/DataSet/Rhythm/Classical"



main :: IO ()
main =
    preprocess
        (load $ dataSetFolder <> "/RhythmTreeBank.json")
        ruleCategory
        getParseTree
        pieceId
        dataSetFolder

-- >>> main


