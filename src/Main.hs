import Compression.SLFP
import Data.Aeson
import Data.List
import Data.Map hiding (drop, take)
import qualified Data.Map as Map hiding (drop)
import qualified Data.Ord
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Tree
import Diagrams
import Diagrams.Backend.SVG (renderSVG)
import Experiment.IsmirExperiment (runAllExperiments, PieceInfo (deCompressionProcess, pieceName))
import Experiment.TismirExperiment
import Grammar.JazzHarmony.JazzGrammar
import Prettyprinter (Pretty (..))
import RIO.FilePath (takeDirectory)
import System.Environment (getExecutablePath)
import Visualization.Text (drawText)
import Visualization.Tree (treeDiagram)
import qualified VisualHTML.Main as VisualHTML
import System.Directory (setCurrentDirectory)

type PatternID = String

type PieceID = String

plotHighlighted :: (_) => IO ()
plotHighlighted = do
    mainRoot <- getExecutablePath
    let repoRoot = takeDirectory . takeDirectory $ mainRoot
    Just (highlighted :: [(PatternID, Map PieceID (Tree (Bool, Abstraction RuleNames)))]) <-
        decodeFileStrict $
            repoRoot <> "/Experiment/Result/patternHighlightedInCorpus.json"

    let diagram =
            hsep 1 $
                uncurry drawOccuranceInCorpus
                    <$> (take 10 . drop 5)
                        (sortOn (Data.Ord.Down . Map.size . snd) highlighted)
    renderSVG
        (repoRoot <> "/Experiment/Result/" <> "highlightedPattern.svg")
        (mkWidth 1000)
        diagram

plotPatternLocs :: (_) => IO ()
plotPatternLocs = do
    mainRoot <- getExecutablePath
    let repoRoot = takeDirectory . takeDirectory $ mainRoot
    Just (locs :: Map PieceID (Tree ([PatternID], Abstraction RuleNames))) <-
        decodeFileStrict $
            repoRoot <> "/Experiment/Result/Harmony/patternLocs.json"

    Just finalSLFP <-
        decodeFileStrict @(SLFP RuleNames PieceID) $
            repoRoot <> "/Experiment/Result/Harmony/finalSLFP.json"

    Just pieceInfos <-
        decodeFileStrict @[PieceInfo PieceID RuleNames] $
            repoRoot <> "/Experiment/Result/Harmony/pieceInfo.json"
    let piecesToShow = ["(Valid)Solar"]
    let pieceTreeCompressed   =
            compressedTree . (debugLookup $ sltps finalSLFP ) 
    let treeToShow pieceTitle = debugLookup locs pieceTitle
    let decompressions k = pieceDecompressProcessAttributed finalSLFP
            Map.! k
    let globalPatternUsed titles =
            foldMap
                ( foldMap
                    ( Set.fromList
                        . Data.List.filter
                            (`Map.member` globalPatterns finalSLFP)
                        . fst
                    )
                    . treeToShow
                )
                titles
    -- let globalMetaUsed titles =
    --         foldMap
    --             ( foldMap
    --                 ( Set.fromList
    --                     . Data.List.filter
    --                         (`Map.member` globalMetas finalSLFP)
    --                     . fst
    --                 )
    --                 . treeToShow
    --             )
    --             titles
    let patternAppendix titles =
            hsep 1 $
                ( \x ->
                    vsep
                        1
                        [ drawText x,
                        treeDiagram (drawText . show . pretty)
                        ((\(TreePattern t) -> t) $ globalPatterns finalSLFP Map.! x)
                        , treeDiagram
                            (drawText . show . pretty)
                            (patternAsComputation finalSLFP x)
                        ]
                )
                    <$> Set.toList (globalPatternUsed titles)
    -- let metaAppendix titles = 
    --         hsep 1 $ 
    --         ( \x ->
    --             vsep
    --                 1
    --                 [ drawText x
    --                 ,
    --                     (drawText . show . pretty)
    --                     (globalMetas finalSLFP Map.! x)
    --                 ]
    --         ) <$>
    --             Set.toList (globalMetaUsed titles)
    
    let drawNode (x, y) =
            vsep
                1
                [ drawText . show . pretty $ y
                , (drawText . show) x
                ]
                # frame 1
                # centerY
    let pieceDiagram pieceTitle =
            vsep
                1
                [ drawText pieceTitle
                , hsep 1 $ treeDiagram 
                    drawNode
                    <$> decompressions pieceTitle
                    -- (pieceTreeCompressed pieceTitle)
                , treeDiagram
                    drawNode
                    (treeToShow pieceTitle)
                ]
    let diagram =
            vsep
                1
                [ hsep 1 $ pieceDiagram <$> piecesToShow
                , patternAppendix piecesToShow
                ]
    renderSVG
        (repoRoot <> "/Experiment/Result/Harmony/" <> "PatternLocs.svg")
        (mkWidth 1000)
        diagram

main :: IO ()
main = do
    mainRoot <- getExecutablePath
    let repoRoot = takeDirectory $ takeDirectory mainRoot
    setCurrentDirectory repoRoot
    runAllExperiments "experiment"
    VisualHTML.main
    plotPatternLocs

-- plotHighlighted
