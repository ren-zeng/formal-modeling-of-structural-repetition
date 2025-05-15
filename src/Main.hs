import Compression.SLFP
import Data.Aeson
import Data.Map hiding (drop, take)
import qualified Data.Map as Map hiding (drop)
import Data.Tree
import Diagrams
import Diagrams.Backend.SVG (renderSVG)
import Experiment.TismirExperiment
import Grammar.JazzHarmony.JazzGrammar
import RIO.FilePath (takeDirectory)
import System.Environment (getExecutablePath)
import Data.List
import qualified Data.Ord
import Experiment.IsmirExperiment (runAllExperiments)



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

main :: IO ()
main = do
    mainRoot <- getExecutablePath
    let repoRoot = takeDirectory $ takeDirectory  mainRoot
    runAllExperiments (repoRoot <> "/Experiment")
    -- plotHighlighted

