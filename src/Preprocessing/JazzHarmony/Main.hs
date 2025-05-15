module Preprocessing.JazzHarmony.Main where

import Control.Arrow
import Core.ParseTree
import Data.Aeson (encodeFile)
import Data.Maybe
import Grammar.JazzHarmony.JazzGrammar
import Grammar.JazzHarmony.MusicTheory
import Preprocessing.JazzHarmony.TreeBankLoader (Piece (title), parsePieces)
import RIO.FilePath (takeDirectory)

treebankPath :: String
treebankPath = "Experiment/DataSet/Harmony/treebank.json"

proofTreeFolderPath :: String
proofTreeFolderPath = "Experiment/DataSet/Harmony/ProofTrees"

titleWithParseTree ::
    Piece ->
    Maybe (String, ParseTree RuleNames ChordLabel ChordLabel)
titleWithParseTree p = do
    t <- pieceParseTree p
    return (title p, t)

main :: IO ()
main = do
    ps <- parsePieces treebankPath
    encodeFile
        (takeDirectory treebankPath <> "/ParseTrees.json")
        (mapMaybe titleWithParseTree ps)
    plotAllProofTree ps proofTreeFolderPath

-- >>> main
