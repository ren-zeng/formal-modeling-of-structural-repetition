module Preprocessing.Main where
import Preprocessing.JazzGrammar (plotAllProofTree)
import Preprocessing.TreeBankParser (parsePieces)

treebankPath :: String
treebankPath = "experiment/data/treebank.json"

proofTreeFolderPath :: String
proofTreeFolderPath = "experiment/data/ProofTrees"

main :: IO () 
main = do 
    ps <- parsePieces treebankPath
    plotAllProofTree ps proofTreeFolderPath 

-- >>> main
