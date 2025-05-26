module VisualHTML.MakeHTML where

import qualified Data.Text.IO as TIO
import qualified Data.Text as T
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TLE
import Text.Mustache
import Data.Aeson (decode, encode, Value (..))

{- |
    Generates an HTML file from a Mustache template and a configuration object.
    The generated HTML file is saved in the specified output directory.

    - `templatePath`: Path to the Mustache template file.
    - `config`: Configuration object to be used in the template (should include dataSet field).
    - `outPath`: Output directory where the generated HTML file will be saved.
-}
mkHTML :: (ToMustache a) => FilePath -> a -> FilePath -> IO ()
mkHTML templatePath config outPath = do
    -- Read the template file
    eitherTemplate <- compileTemplate templatePath <$> TIO.readFile templatePath
    case eitherTemplate of
        Left e -> error $ show e
        Right template -> do
            let generatedHTML = substitute template config
            TIO.writeFile outPath generatedHTML