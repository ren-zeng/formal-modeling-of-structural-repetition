{-# LANGUAGE TupleSections #-}

module Experiment.IsmirExperiment where

import Data.Maybe

import Compression.Meta
import Compression.SLFP
import Data.Aeson
import Data.List (sortOn)
import qualified Data.Map as Map hiding (mapMaybe, take)
import GHC.Generics hiding (Meta)
import Grammar.JazzHarmony.JazzGrammar
import Prettyprinter

-- import Visualization.TreeVisualizer

import AnalyzePattern (SLFPBinding (patternLookup))
import Compression.InlineDigram
import Core.ParseTree
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Tree
import Debug.Trace (traceM, traceWith)
import Experiment.TismirExperiment
import Grammar.JazzHarmony.MusicTheory
import Grammar.Rhythm.RhythmGrammar (RhythmNT, RhythmRule, RhythmTerminal)
import Preprocessing.Preprocess
import Safe (readMay)
import Data.Function
import Data.Functor.Base
import Data.Functor.Foldable

proofTreeFolderPath :: String
proofTreeFolderPath = "experiment/data/ProofTrees"

minedMetas :: SLFP r b -> [(String, Meta)]
minedMetas = Map.toList . globalMetas

-- filter (\(_, m) -> length m <= 4)
-- . sortOn (metaRuleNameToInt . fst)

-- >>> (minedMetas . fixedPoint compressG . harmonyCorpuSLFP) <$> pieces

-- was
-- [("RG1",[_,0]),("RG4",[★,_]),("RG9",[_,_,0,1]),("RG13",[★]),("RG16",[_,★]),("RG17",[_,0,0,0]),("RG20",[_,_,0,_]),("RG27",[_,_,_,1]),("RG62",[_,0,_,0]),("RG63",[_,0,_,_]),("RG64",[_,0,_]),("RG65",[_,_,_,0]),("RG100",[_,_,0,0]),("RG101",[_,_,_,2]),("RG148",[_,_,1,_])]

metaRuleNameToInt :: String -> Int
metaRuleNameToInt = f . drop 2
  where
    f x = case readMay x of
        Just n -> n
        Nothing -> error $ "read Int fail: " <> x

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

-- data SizeChange = SizeChange
--     { pieceName :: String
--     , originalSize :: Int
--     , compressedSize :: Int
--     }
--     deriving (Generic, Show)

-- instance ToJSON SizeChange
-- instance FromJSON SizeChange

data PieceInfo k r = PieceInfo
    { pieceName :: k
    , originalSize :: Int
    , compressedSize :: Int
    , deCompressionProcess :: [Tree (Abstraction r)]
    }
    deriving (Generic, Show)

instance (ToJSON k, ToJSON r) => ToJSON (PieceInfo k r)
instance (FromJSON k, FromJSON r) => FromJSON (PieceInfo k r)

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
        final' = last steps
        inlined = metaStageCompress show final'
      
        freq = patternFreqInCorpus inlined
            & traceWith (show . pretty. Map.toList)

        insufficientPatterns = globalPatterns inlined
            & Map.filterWithKey
                (\k _ ->
                    k `Map.notMember` freq
                -- &&
                -- null (patternDependents inlined k)
                )

            & traceWith (\x -> "insufficientPatterns: " <> show ( Map.toList x))

        patternsAfterPruning = (globalPatterns inlined
            Map.\\ insufficientPatterns)
            & traceWith (\x -> "patternsAfterPruning: "<> (show. pretty . Map.toList $ fmap (Set.toList . varsInPattern ) x))
        varsInTreePattern :: Tree (Abstraction a) -> Set String
        varsInTreePattern = cata $ \case
            NodeF a [] -> getVariables a
            NodeF a rs -> Set.unions (getVariables a:rs)

        varsInPattern :: Pattern (Abstraction a) -> Set String
        varsInPattern = \case
            Comp i x y -> getVariables x `Set.union` getVariables y
            TreePattern t -> varsInTreePattern t


        final =
            inlined
                { globalPatterns = patternsAfterPruning
                }
            -- & traceWith (\x -> "allPiecesTrees: "<> (show. pretty . Map.toList $ fmap ((Set.toList . varsInTreePattern) . compressedTree) (sltps x)))


    putStrLn "finished compression"

    putStrLn "saving finalSLFP"
    encodeFile (resultDir <> "/finalSLFP.json") final

    putStrLn "saving globalMetas"
    encodeFile (resultDir <> "/globalMetas.json") (minedMetas final)



    putStrLn "computing patternInfo"
    let patternInfo = mkPatternInfo final $ Map.keys (globalPatterns final)

    putStrLn "saving patternInfo"
    encodeFile (resultDir <> "/patternInfo.json") patternInfo

    putStrLn "saving patternLocs"
    encodeFile (resultDir <> "/patternLocs.json") $ markPatternIdInCorpus final

    putStrLn "computing pieceInfos"
    let pieceInfos =
            fmap (\(k, ((m, n), ts)) -> PieceInfo k m n ts) $
                Map.toList $
                    Map.intersectionWith
                        (,)
                        (individualPieceChange slfp final)
                        (pieceDecompressProcess final)

    putStrLn "saving pieceInfos"
    encodeFile (resultDir <> "/pieceInfo.json") pieceInfos

    putStrLn "saving ruleStats"
    encodeFile (resultDir <> "/ruleStats.json") $ ruleSummary final

    putStrLn "saving sizeCurve"
    encodeFile
        (resultDir <> "/sizeCurve.json")
        (uncurry SizeCurve <$> zip [1 ..] (size <$> steps ++ [final]))





type PatternID = String

data PatternInfo r k = PatternInfo
    { patternID :: PatternID
    , definition :: Pattern (Abstraction r)
    , dependentsDirect :: Set PatternID
    , dependenciesDirect :: Set PatternID
    , globalFreq :: Int
    , sizeExpanded :: Int
    , occuranceInCorpus :: Set k
    , impact :: Double
    , depths :: [Double]
    , ruleTree :: Tree (Abstraction r)
    }
    deriving (Generic, Show)

instance (ToJSON k, ToJSON r) => ToJSON (PatternInfo r k)
instance (FromJSON k, FromJSON r, Ord k) => FromJSON (PatternInfo r k)

mkPatternInfo :: (_) => SLFP r k -> [PatternID] -> [PatternInfo r k]
mkPatternInfo slfp pIds =
    let
        dFreq = patternFreqInCorpus slfp
        globalFrequency k = fromMaybe 0 (Map.lookup k dFreq)

     in
        ( \pId ->
            PatternInfo
                { patternID = pId
                , definition = globalPatterns slfp Map.! pId
                , dependentsDirect = directDependents slfp pId
                , dependenciesDirect = directDependencies slfp pId
                , globalFreq = globalFrequency pId
                , sizeExpanded = patternSizeExpanded slfp pId
                , occuranceInCorpus = patternOccuranceG slfp pId
                , depths = patternDepthInCorpus slfp Map.! pId
                , impact = patternImpact globalFrequency (Set.toList . directDependents slfp) pId
                , ruleTree = patternAsComputation slfp pId
                }
        )
            <$> pIds

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
    (FilePath -> IO (Either String [(k, ParseTree r nt t)])) ->
    FilePath ->
    FilePath ->
    IO ()
runExperiment readRuleTree inputFile outputDir = do
    psEither <- readRuleTree inputFile
    case psEither of
        Left e -> print $ e
        Right ps -> reportCompression outputDir $ initSLFP (f ps)
  where
    f = mapMaybe (\(name, pt) -> (name,) <$> g pt) -- fmap $ fmap  $ second parseTreeToRuleTree
    g = parseTreeToRuleTree

jazzHarmonyExperiment :: FilePath -> FilePath -> IO ()
jazzHarmonyExperiment =
    runExperiment
        ( eitherDecodeFileStrict
            @[(String, ParseTree (Maybe RuleNames) ChordLabel ChordLabel)]
        )

rhythmExperiment :: FilePath -> FilePath -> IO ()
rhythmExperiment =
    runExperiment
        ( eitherDecodeFileStrict
            @[(String, ParseTree (Maybe RhythmRule) RhythmNT RhythmTerminal)]
        )

runAllExperiments :: FilePath -> IO ()
runAllExperiments experimentFolder = do
    jazzHarmonyExperiment
        (experimentFolder <> "/DataSet/Harmony/ParseTrees.json")
        (experimentFolder <> "/Result/Harmony")
    rhythmExperiment
        (experimentFolder <> "/DataSet/Rhythm/Classical/ParseTrees.json")
        (experimentFolder <> "/Result/Rhythm/Classical")
