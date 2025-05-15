module AnalyzePattern where

import Compression.Meta
import Compression.SLFP
import Data.Tree


{-
This module provides helper functions for analyzing Patterns encoded in SLFP
The kind of analysis includes:
- 1. expanding a pattern from variable to its expanded out form as a tree fragment
-}

data SLFPBinding a = SLFPBinding
    { patternLookup :: String -> Pattern (Abstraction a)
    , metaLookUp :: String -> Meta
    , arityLookUp :: Abstraction a -> Int
    }

expandPattern :: _ =>
    SLFPBinding a ->
    String ->
    Tree (Abstraction a)
expandPattern binding x = fixedPoint (deCompressTree 
    (patternLookup binding)
    (metaLookUp binding)
    (arityLookUp binding))
    (Node (Var x) []) 



 
