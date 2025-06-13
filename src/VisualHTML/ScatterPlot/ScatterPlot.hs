module VisualHTML.ScatterPlot.ScatterPlot where

import Control.Exception

import Data.Text (Text)
import qualified Data.Text.IO as TIO
import Prettyprinter (Doc)
import Text.Mustache
import qualified Text.Mustache.Compile as ScatterPlot
import Text.Printf
import VisualHTML.MakeHTML

type D3JSCode = String

data ScatterPlotConfig = ScatterPlotConfig
    { figureName :: String
    , dataSet :: Text
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

mkScatterConfigCompression :: String -> FilePath -> IO ScatterPlotConfig
mkScatterConfigCompression x path = do
    json <- TIO.readFile path
    return
        ScatterPlotConfig
            { figureName = x
            , dataSet = json
            , xLabel = "original size"
            , yLabel = "compressed size"
            , _X = "originalSize"
            , _Y = "compressedSize"
            , _hoverInfo = "pieceName"
            }

mkScatterConfigFreqSize :: String -> FilePath -> IO ScatterPlotConfig
mkScatterConfigFreqSize x path = do
    json <- TIO.readFile path
    return
        ScatterPlotConfig
            { figureName = x
            , dataSet = json
            , xLabel = "frequency"
            , yLabel = "size"
            , _X = "globalFreq"
            , _Y = "sizeExpanded"
            , _hoverInfo = "patternID"
            }

plotScatterCompression :: ScatterPlotConfig -> FilePath -> IO ()
plotScatterCompression =
    mkHTML
        "src/VisualHTML/ScatterPlot/ScatterPlot.mustache"

readPlotScatter ::
    (String -> FilePath -> IO ScatterPlotConfig) ->
    String ->
    FilePath ->
    FilePath ->
    IO ()
readPlotScatter mkConfig figTitle dataPath outPath = do
    config <- mkConfig figTitle dataPath
    plotScatterCompression config outPath

main = do
    readPlotScatter mkScatterConfigCompression
        "Piece-wise compression (Harmony)"
        "Experiment/Result/Harmony/pieceInfo.json"
        "Experiment/Result/Harmony/CompressionScatterPlot(Harmony).html"

    readPlotScatter mkScatterConfigCompression
        "Piece-wise compression (Rhythm)"
        "Experiment/Result/Rhythm/Classical/pieceInfo.json"
        "Experiment/Result/Rhythm/Classical/CompressionScatterPlot(Rhythm).html"

    readPlotScatter mkScatterConfigFreqSize 
        "Frequency vs Size (Harmony)"
        "Experiment/Result/Harmony/patternInfo.json"
        "Experiment/Result/Harmony/patternFreqSize(Harmony).html"

    readPlotScatter mkScatterConfigFreqSize 
        "Frequency vs Size (Rhythm)"
        "Experiment/Result/Rhythm/Classical/patternInfo.json"
        "Experiment/Result/Rhythm/Classical/patternFreqSize(Rhythm).html"
    

-- >>> main
