module VisualHTML.BarPlot.BarPlot where
import Text.Mustache
import VisualHTML.MakeHTML (mkHTML)

data BarPlotConfig = BarPlotConfig
    { figureName :: String
    , dataSet :: FilePath
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

mkBarPlotConfigRuleReport :: String -> FilePath -> BarPlotConfig
mkBarPlotConfigRuleReport x path =
    BarPlotConfig
        { figureName = x
        , dataSet = path
        , xLabel = "Rule Name"
        , yLabel = "Frequency"
        , _X = "feature"
        , _Y = "frequency"
        , _hoverInfo = ""
        }

plotBarPlot x path = mkHTML 
    "src/VisualHTML/BarPlot/BarPlot.mustache" 
    "RuleDistribution" 
    (mkBarPlotConfigRuleReport x path)

main = do
    mapM_
        (plotBarPlot 
        "RuleDistribution" 
        "RuleDistribution.json")
        ["Experiment/DataSet/Rhythm/Classical", "Experiment/DataSet/Harmony"]

-- >>> main
