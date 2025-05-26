module VisualHTML.SvgToPdf (
    convertSvgsToPdf,
    convertSvgToPdf
) where

import System.Process (callProcess)
import System.Directory 
import System.FilePath (replaceExtension, takeFileName)
import Control.Monad (unless)
import Control.Concurrent.Async (mapConcurrently_)
import Control.Exception (try, SomeException)

-- | Convert multiple SVG files to PDF concurrently
convertSvgsToPdf :: [FilePath] -> FilePath -> IO ()
convertSvgsToPdf svgPaths outDir = do
    -- Create output directory if it doesn't exist
    createDirectoryIfMissing True outDir
    -- Convert files concurrently, discarding results since we only care about the effects
    mapConcurrently_ (convertSvgToPdfSafe outDir) svgPaths

-- | Safe version of convertSvgToPdf that handles errors
convertSvgToPdfSafe :: FilePath -> FilePath -> IO ()
convertSvgToPdfSafe outDir svgPath = do
    result <- try $ convertSvgToPdf svgPath outDir
    case result of
        Left e -> 
            putStrLn $ "Error converting " ++ svgPath ++ ": " ++ show (e :: SomeException)
        Right _ -> return ()

-- | Convert a single SVG file to PDF using rsvg-convert
-- Returns the path to the generated PDF file
convertSvgToPdf :: FilePath -> FilePath -> IO FilePath
convertSvgToPdf svgPath outDir= do
    -- Check if SVG file exists
    exists <- doesFileExist svgPath
    unless exists $ error $ "SVG file does not exist: " ++ svgPath
    
    -- Generate PDF path by replacing extension
    let filename = replaceExtension (takeFileName svgPath) ".pdf"
    let pdfPath = outDir <> "/" <>filename
    -- Convert using rsvg-convert
    callProcess "rsvg-convert" [
        "-f", "pdf",  -- output format
        "-o", pdfPath,  -- output file
        svgPath  -- input file
        ]
    
    return pdfPath



