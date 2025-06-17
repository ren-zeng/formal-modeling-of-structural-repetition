module VisualHTML.Main where 
import qualified VisualHTML.StackedBarPlot.StackedBarPlot as StackedBarPlot
import qualified VisualHTML.ScatterPlot.ScatterPlot as ScatterPlot
import qualified VisualHTML.DependencyPlot.DependencyPlot as DependencyPlot

main :: IO ()
main = do 
    StackedBarPlot.main 
    ScatterPlot.main 
    DependencyPlot.main