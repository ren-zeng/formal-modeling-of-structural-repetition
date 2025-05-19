module VisualHTML.ScatterPlot where

import Control.Exception

import qualified Data.Text.IO as TIO
import Prettyprinter (Doc)
import Text.Mustache
import Text.Printf

type D3JSCode = String

data ScatterPlotConfig = ScatterPlotConfig
    { figureName :: String
    , dataSet :: FilePath
    , xLabel :: String
    , yLabel :: String
    , _X :: String -- field name for x coordinate
    , _Y :: String -- field name for y coordinate
    , _hoverInfo :: String -- field name for info when hovering
    }

instance ToMustache ScatterPlotConfig where
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

mkScatterConfigCompression :: String -> FilePath -> ScatterPlotConfig
mkScatterConfigCompression x path =
    ScatterPlotConfig
        { figureName = x
        , dataSet = path
        , xLabel = "originalSize"
        , yLabel = "compressedSize"
        , _X = "originalSize"
        , _Y = "compressedSize"
        , _hoverInfo = "pieceName"
        }

plotScatterCompression :: (_) => String -> FilePath -> FilePath -> IO ()
plotScatterCompression figName dataPath outDir = do
    let templateFile = "src/VisualHTML/ScatterPlot.mustache"
    let config = mkScatterConfigCompression figName dataPath
    eitherTemplate <- compileTemplate templateFile <$> TIO.readFile templateFile
    case eitherTemplate of
        Left e -> error $ show e
        Right template -> do
            let generatedHTML = substitute template config
            TIO.writeFile (outDir <> "/" <> figureName config <> ".html") generatedHTML

main = do
    mapM_
        (plotScatterCompression 
        "compressionScatterPlot" 
        "pieceSizeComparison.json")
        ["Experiment/Result/Rhythm/Classical", "Experiment/Result/Harmony"]

-- >>> main
