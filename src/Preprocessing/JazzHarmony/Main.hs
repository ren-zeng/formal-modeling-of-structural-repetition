module Preprocessing.JazzHarmony.Main where

import Core.ParseTree
import Data.Aeson.Types
import Grammar.JazzHarmony.JazzGrammar
import Grammar.JazzHarmony.MusicTheory
import Preprocessing.JazzHarmony.TreeBankLoader
import Preprocessing.Preprocess

getParseTree :: Piece -> ParseTree (Maybe RuleNames) ChordLabel ChordLabel
getParseTree = inferRuleTree inferRule . getSymbolTree

load :: FilePath -> IO ([String],[Piece])
load path = do
    Just r <- decodeTreeBank path
    let ps = [p | Success p <- r]
    let errors = [e | Error e <- r]
    return (errors,ps)

main :: IO ()
main =
    preprocess
        (load "Experiment/DataSet/Harmony/treebank.json")
        ruleCategory
        getParseTree
        title
        "Experiment/DataSet/Harmony"

-- >>> main
