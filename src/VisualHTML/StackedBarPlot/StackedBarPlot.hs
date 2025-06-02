module VisualHTML.StackedBarPlot.StackedBarPlot where

import Text.Mustache
import VisualHTML.MakeHTML (mkHTML)
import qualified Data.Text.IO as TIO
import Data.Text (Text)

-- Configuration for stacked bar plots with category-based data
data StackedBarPlotConfig = StackedBarPlotConfig
    { figureName :: String
    , dataSet :: Text       -- JSON data with multiple values per category
    , xLabel :: String      -- Label for the feature axis (usually vertical in horizontal bar plots)
    , yLabel :: String      -- Label for the frequency axis
    , _X :: String          -- Field name for features (e.g., "feature")
    , _Y :: String          -- Field name for values (e.g., "frequency")
    , _C :: String          -- Field name for categories (e.g., "category")
    }

instance ToMustache StackedBarPlotConfig where
    toMustache x =
        object
            [ "figureName" ~> figureName x
            , "dataSet" ~> dataSet x
            , "xLabel" ~> xLabel x
            , "yLabel" ~> yLabel x
            , "_X" ~> _X x
            , "_Y" ~> _Y x
            , "_C" ~> _C x
            ]



-- Function to create a custom stacked bar plot configuration
createStackedBarPlotConfig :: String -> FilePath  -> IO StackedBarPlotConfig
createStackedBarPlotConfig plotName dataPath  = do
    json <- TIO.readFile dataPath
    return $ StackedBarPlotConfig
        { figureName = plotName
        , dataSet = json
        , xLabel = "Rule Category"
        , yLabel = "Frequency"
        , _X = "ruleName"
        , _Y = "occurancesInCorpus"
        , _C = "ruleCategory"
        }

-- Main function to generate the stacked bar plot HTML file
plotStackedBarPlot :: StackedBarPlotConfig -> FilePath -> IO ()
plotStackedBarPlot = mkHTML "src/VisualHTML/StackedBarPlot/StackedBarPlot.mustache"

-- Example usage function
-- The input JSON should be formatted as an array of objects where each object has:
-- - feature: the name of the feature/rule
-- - frequency: the numerical value
-- - category: which category/group this data point belongs to
-- For example: [{"feature": "Rule1", "frequency": 10, "category": "group1"}, ...]
exampleMain :: IO ()
exampleMain = do
    rhythmConfig <- createStackedBarPlotConfig
        "Classical Rhythm Rule Distribution (Stacked)"
        "Experiment/DataSet/Rhythm/Classical/RuleInfo.json"
    plotStackedBarPlot
        rhythmConfig
        "Experiment/DataSet/Rhythm/Classical/StackedRuleDistribution.html"
    
    harmonyConfig <- createStackedBarPlotConfig 
        "Jazz Harmony Rule Distribution (Stacked)"
        "Experiment/DataSet/Harmony/RuleInfo.json"
    plotStackedBarPlot
        harmonyConfig
        "Experiment/DataSet/Harmony/StackedRuleDistribution.html"

-- >>> exampleMain

