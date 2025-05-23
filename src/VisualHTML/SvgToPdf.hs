module VisualHTML.SvgToPdf where

import System.Process (callProcess)
import System.Directory 
import System.FilePath (replaceExtension, takeFileName)
import Control.Monad (when, unless)
import Control.Concurrent.Async (mapConcurrently)
import Control.Exception (try, SomeException)

-- | Convert multiple SVG files to PDF concurrently
-- Returns a list of generated PDF file paths
convertSvgsToPdf :: [FilePath] -> FilePath -> IO [FilePath]
convertSvgsToPdf svgPaths outDir = do
    -- Create output directory if it doesn't exist
    createDirectoryIfMissing True outDir
    -- Convert files concurrently and collect results
    mapConcurrently (convertSvgToPdfSafe outDir) svgPaths

-- | Safe version of convertSvgToPdf that handles errors
convertSvgToPdfSafe :: FilePath -> FilePath -> IO FilePath
convertSvgToPdfSafe outDir svgPath = do
    result <- try $ convertSvgToPdf svgPath outDir
    case result of
        Left e -> do
            putStrLn $ "Error converting " ++ svgPath ++ ": " ++ show (e :: SomeException)
            return ""
        Right path -> return path

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



