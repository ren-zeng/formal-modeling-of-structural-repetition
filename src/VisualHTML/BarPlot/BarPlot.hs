module VisualHTML.BarPlot.BarPlot where

import Text.Mustache
import VisualHTML.MakeHTML (mkHTML)
import qualified Data.Text.IO as TIO
import Data.Text (Text)

data BarPlotConfig = BarPlotConfig
    { figureName :: String
    , dataSet :: Text
    , xLabel :: String
    , yLabel :: String
    , _X :: String -- field name for x value
    , _Y :: String -- field name for y value
    , _hoverInfo :: String -- field name for info when hovering
    }

instance ToMustache BarPlotConfig where
    toMustache x =
        object
            [ "figureName" ~> figureName x
            , "dataSet" ~> dataSet x
            , "xLabel" ~> xLabel x
            , "yLabel" ~> yLabel x
            , "_X" ~> _X x
            , "_Y" ~> _Y x
            , "_hoverInfo" ~> _hoverInfo x
            ]

configRuleReport :: String -> FilePath -> IO BarPlotConfig
configRuleReport plotName dataPath = do 
    json <- TIO.readFile dataPath
    return $ BarPlotConfig
        { figureName = plotName
        , dataSet = json
        , xLabel = "Rule Name"
        , yLabel = "Frequency"
        , _X = "ruleName"
        , _Y = "occurancesInCorpus"
        , _hoverInfo = ""
        }

plotBarPlot :: BarPlotConfig -> FilePath -> IO ()
plotBarPlot = mkHTML "src/VisualHTML/BarPlot/BarPlot.mustache"

main = do
    rhythmConfig <- configRuleReport
        "Classical Rhythm Rule Distribution"
        "Experiment/DataSet/Rhythm/Classical/RuleInfo.json"
    plotBarPlot
        rhythmConfig
        "Experiment/DataSet/Rhythm/Classical/RuleDistribution.html"
    
    
    harmonyConfig <- configRuleReport 
        "Jazz Harmony Rule Distribution"
        "Experiment/DataSet/Harmony/RuleInfo.json"
    plotBarPlot
        harmonyConfig
        "Experiment/DataSet/Harmony/RuleDistribution.html"

-- >>> main
