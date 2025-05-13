import Compression.SLFP
import Data.Aeson
import Data.Map hiding (drop, take)
import qualified Data.Map as Map hiding (drop)
import Data.Tree
import Diagrams
import Diagrams.Backend.SVG (renderSVG)
import Diagrams.Prelude (black, brown)
import Experiment.IsmirExperiment (ismirExperiment)
import Experiment.TismirExperiment
import Preprocessing.JazzGrammar
import Prettyprinter (pretty)
import RIO.Directory (getCurrentDirectory, getHomeDirectory)
import RIO.FilePath (takeDirectory)
import System.Environment (getExecutablePath)
import Visualization.BackEnd (BackEnd)
import Visualization.Text
import Data.List
import qualified Data.Ord

main :: IO ()
main = do
    -- ismirExperiment
    plotHighlighted

type PatternID = String

type PieceID = String

plotHighlighted :: (_) => IO ()
plotHighlighted = do
    mainRoot <- getExecutablePath
    let repoRoot = takeDirectory . takeDirectory $ mainRoot
    Just (highlighted :: [(PatternID, Map PieceID (Tree (Bool, Abstraction RuleNames)))]) <-
        decodeFileStrict $
            repoRoot <> "/Experiment/Result/patternHighlightedInCorpus.json"

    
    let diagram = hsep 1 $ uncurry drawOccuranceInCorpus <$> (take 10 . drop 5)
            (sortOn (Data.Ord.Down . Map.size . snd) highlighted)
    renderSVG
        (repoRoot <> "/Experiment/Result/" <> "highlightedPattern.svg")
        (mkWidth 1000)
        diagram
