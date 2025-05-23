module Preprocessing.JazzHarmony.Main where

import Core.ParseTree
import Data.Aeson.Types
import Grammar.JazzHarmony.JazzGrammar
import Grammar.JazzHarmony.MusicTheory
import Preprocessing.JazzHarmony.TreeBankLoader
import Preprocessing.Preprocess

getParseTree :: Piece -> ParseTree (Maybe RuleNames) ChordLabel ChordLabel
getParseTree = inferRuleTree inferRule . getSymbolTree

load :: FilePath -> IO [Piece]
load path = do
    Just r <- decodeTreeBank path
    let ps = [p | Success p <- r]
    return ps

main :: IO ()
main =
    preprocess
        (load "Experiment/DataSet/Harmony/treebank.json")
        getParseTree
        title
        "Experiment/DataSet/Harmony"

-- >>> main
-- SVG file does not exist: (Valid)Fools Rush In.svg
