{-# LANGUAGE TupleSections #-}

module Experiment.IsmirExperiment where

import Data.Maybe

import Compression.Meta
import Compression.SLFP
import Data.Aeson
import Data.List (sortOn)
import qualified Data.Map as Map hiding (filter, mapMaybe, take)
import Data.Tree
import GHC.Generics hiding (Meta)
import Preprocessing.JazzGrammar
import Preprocessing.TreeBankParser
import Prettyprinter
import RIO.FilePath
import System.Environment (getExecutablePath)

import AnalyzePattern

-- import Visualization.TreeVisualizer

import Control.Arrow
import Control.Monad
import Debug.Trace (traceM)
import Diagrams hiding (size)
import Diagrams.Backend.SVG
import Experiment.TismirExperiment 
import Visualization.Text
import Visualization.Tree

proofTreeFolderPath :: String
proofTreeFolderPath = "experiment/data/ProofTrees"

mkCorpusSLFP :: (_) => (a -> b) -> (a -> Maybe (Tree r)) -> [a] -> SLFP r b
mkCorpusSLFP getName getTree xs = initSLFP $ mapMaybe f xs
  where
    f x = do
        tree <- getTree x
        return (getName x, tree)

harmonyCorpuSLFP :: (_) => [Piece] -> SLFP RuleNames String
harmonyCorpuSLFP = mkCorpusSLFP title tRule

minedMetas :: SLFP r b -> [(String, Meta)]
minedMetas = filter (\(_, m) -> length m <= 4) . sortOn (metaRuleNameToInt . fst) . Map.toList . globalMetas

-- >>> (minedMetas . fixedPoint compressG . harmonyCorpuSLFP) <$> pieces

-- was
-- [("RG1",[_,0]),("RG4",[★,_]),("RG9",[_,_,0,1]),("RG13",[★]),("RG16",[_,★]),("RG17",[_,0,0,0]),("RG20",[_,_,0,_]),("RG27",[_,_,_,1]),("RG62",[_,0,_,0]),("RG63",[_,0,_,_]),("RG64",[_,0,_]),("RG65",[_,_,_,0]),("RG100",[_,_,0,0]),("RG101",[_,_,_,2]),("RG148",[_,_,1,_])]

metaRuleNameToInt :: String -> Int
metaRuleNameToInt = read . drop 2

-- >>> metaRuleNameToInt "RG115"
-- 115

ruleSummary :: SLFP a b -> (Int, Int, Int, Int)
ruleSummary slfp =
    ( sum $ Map.size . patterns <$> Map.elems (sltps slfp)
    , sum $ Map.size . metas <$> Map.elems (sltps slfp)
    , Map.size $ globalPatterns slfp
    , Map.size $ globalMetas slfp
    )

-- >>> (ruleSummary . fixedPoint compressG . corpusSLFP) <$> pieces
-- was -- (36,0,198,20)

individualPieceChange :: (Ord k) => SLFP a1 k -> SLFP a2 k -> Map.Map k (Int, Int)
individualPieceChange slfpOrig selfFin = Map.intersectionWith (,) (size <$> sltps slfpOrig) (size <$> sltps selfFin)

data SizeChange = SizeChange
    { pieceName :: String
    , originalSize :: Int
    , compressedSize :: Int
    }
    deriving (Generic, Show)

instance ToJSON SizeChange
instance FromJSON SizeChange

data SizeCurve = SizeCurve
    { step :: Int
    , corpusSize :: Int
    }
    deriving (Generic, Show)

instance ToJSON SizeCurve
instance FromJSON SizeCurve

reportCompression :: (_) => FilePath -> SLFP a b -> IO ()
reportCompression resultDir slfp = do
    traceM $ show $ Map.size $ sltps slfp
    let steps = compressGSteps slfp
        final = last steps
        ms = minedMetas final
        ruleStats = ruleSummary final
        pieceSizeComparison = (\(k, (ori, fin)) -> SizeChange (show $ pretty k) ori fin) <$> Map.toList (individualPieceChange slfp final)

    encodeFile (resultDir <> "globalMetas.json") ms
    encodeFile (resultDir <> "ruleStats.json") ruleStats
    encodeFile (resultDir <> "pieceSizeComparison.json") pieceSizeComparison
    encodeFile
        (resultDir <> "sizeCurve.json")
        (uncurry SizeCurve <$> zip [1 ..] (size <$> steps))

    encodeFile (resultDir <> "patternDependents.json")
        $ report patternDependents final
    encodeFile (resultDir <> "patternGlobalFreq.json")
        $ report patternGlobalFreq final
    encodeFile (resultDir <> "patternOccuranceG.json")
        $ report patternOccuranceG final
    encodeFile (resultDir <> "patternHighlightedInCorpus.json") 
        $ report highlightPatternInCorpus final


-- compressCorpus :: _ => (FilePath -> IO [a]) ->
--     (a -> b) ->
--     (a -> Maybe (Tree r)) ->
--     FilePath -> IO (SLFP r b)
-- compressCorpus readCorpus getName getTree inputFile = do
--     ps <- readCorpus inputFile
--     let slfp = mkCorpusSLFP getName getTree ps
--     traceM $ show slfp
--     let final = fixedPoint compressG slfp
--     return final

-- testExpandPattern :: IO _
-- testExpandPattern = do
--     slfp <- compressCorpus parsePieces
--         title
--         tRule
--         ("./experiment/data/treebank.json")
--     let binding = SLFPBinding
--             (globalPatterns slfp Map.!)
--             (globalMetas slfp Map.!)
--             (arities slfp Map.!)
--     let patternVars = take 5 <$> Map.keys (globalPatterns slfp)
--     forM patternVars $ \x -> do
--         let patternTree = expandPattern binding x
--         let d = treeDiagram drawText (show  <$> patternTree)
--         renderSVG ("src/testTreePattern" <> x <> ".svg") (mkSizeSpec2D (Just 1000) (Just 1000)) d
--         return $ patternTree

runExperiment ::
    (_) =>
    (FilePath -> IO [a]) ->
    (a -> b) ->
    (a -> Maybe (Tree r)) ->
    FilePath ->
    FilePath ->
    IO ()
runExperiment readCorpus getName getTree inputFile outputDir = do
    ps <- readCorpus inputFile
    reportCompression outputDir $ mkCorpusSLFP getName getTree ps

ismirExperiment :: IO ()
ismirExperiment = do
    p <- takeDirectory . takeDirectory <$> getExecutablePath
    runExperiment
        parsePieces
        title
        tRule
        (p <> "/Experiment/DataSet/treebank.json")
        (p <> "/Experiment/Result/")


-- M_{1} &= \langle \_,1 \rangle &

-- plotCompressionExample = do
--     ps <-  pieces
--     let original = corpusSLFP $ take 5 ps
--         final = fixedPoint compressG original
--         decompressed = fixedPoint deCompressG final
--     writeSVG "JazzCorpusCompression(compressed).svg" (plotSLFP final)

--     writeSVG "JazzCorpusCompression(decompressed).svg" (plotSeqDiagrams $ plotSLFP <$> iterateFs [deCompressG |i<- [1 ..] ] final)

-- -- >>>  plotCompressionExample
