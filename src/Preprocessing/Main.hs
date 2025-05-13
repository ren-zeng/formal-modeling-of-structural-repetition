module Preprocessing.Main where
import Preprocessing.JazzGrammar (plotAllProofTree)
import Preprocessing.TreeBankParser (parsePieces)

treebankPath :: String
treebankPath = "Experiment/DataSet/treebank.json"

proofTreeFolderPath :: String
proofTreeFolderPath = "Experiment/DataSet/ProofTrees"

main :: IO () 
main = do 
    ps <- parsePieces treebankPath
    plotAllProofTree ps proofTreeFolderPath 

-- >>> main
