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
import Experiment.IsmirExperiment (runAllExperiments)
import Experiment.TismirExperiment
import Grammar.JazzHarmony.JazzGrammar
import Prettyprinter (Pretty (..))
import RIO.FilePath (takeDirectory)
import System.Environment (getExecutablePath)
import Visualization.Text (drawText)
import Visualization.Tree (treeDiagram)

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
    let piecesToShow = ["(Valid)Solar"]
    let pieceTreeCompressed  =
            compressedTree . (sltps finalSLFP Map.!) 
    let treeToShow pieceTitle = locs Map.! pieceTitle
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
                        [ drawText x
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
                , treeDiagram 
                    (drawText . show . pretty) 
                    (pieceTreeCompressed pieceTitle)
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
    runAllExperiments (repoRoot <> "/Experiment")
    plotPatternLocs

-- plotHighlighted
