module VisualHTML.MakeHTML where
import qualified Data.Text.IO as TIO
import Text.Mustache

mkHTML :: (_) => FilePath -> String -> a -> FilePath -> IO ()
mkHTML templatePath fileName config outDir = do
    -- let templateFile = "src/VisualHTML/ScatterPlot.mustache"
    -- let config = mkScatterConfigCompression figName dataPath
    eitherTemplate <- compileTemplate templatePath <$> TIO.readFile templatePath
    case eitherTemplate of
        Left e -> error $ show e
        Right template -> do
            let generatedHTML = substitute template config
            TIO.writeFile (outDir <> "/" <> fileName <> ".html") generatedHTML