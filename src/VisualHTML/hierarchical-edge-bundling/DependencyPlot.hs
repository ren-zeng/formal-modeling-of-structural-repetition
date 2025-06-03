module VisualHTML.DependencyPlot where
import Text.Mustache
import qualified Data.Text.IO as TIO
import Data.Text (Text)
import VisualHTML.MakeHTML


data DependencyPlotConfig = BarPlotConfig
    { title :: String
    , dataSet :: Text
    }

instance ToMustache DependencyPlotConfig where
    toMustache x =
        object
            [ "title" ~> title x
            , "dataSet" ~> dataSet x
            ]


configPatternDependency :: String -> FilePath -> IO DependencyPlotConfig
configPatternDependency plotName dataPath = do 
    json <- TIO.readFile dataPath
    return $ BarPlotConfig
        { title = plotName
        , dataSet = json
        }

plotBarPlot :: DependencyPlotConfig -> FilePath -> IO ()
plotBarPlot = mkHTML "src/VisualHTML/hierarchical-edge-bundling/HierarchicalEdgeBundling.mustache"

main = do
    rhythmConfig <- configPatternDependency
        "Classical Rhythm Pattern Dependency"
        "experiment/Result/Rhythm/Classical/patternInfo.json"
    plotBarPlot
        rhythmConfig
        "experiment/Result/Rhythm/Classical/patternDependency.html"
    
    
    harmonyConfig <- configPatternDependency 
        "Jazz Harmony Pattern Dependency"
        "experiment/Result/Harmony/patternInfo.json"
    plotBarPlot
        harmonyConfig
        "experiment/Result/Harmony/patternDependency.html"

-- >>> main
