{-# LANGUAGE BlockArguments #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use &&" #-}
{-# HLINT ignore "Use ||" #-}

module Grammar.JazzHarmony.JazzGrammar where

import Data.Aeson
import GHC.Generics (Generic)
import Grammar.JazzHarmony.MusicTheory
import Musicology.Pitch

import Control.Monad
import Core.ParseTree (ParseTree (..), inferParseTree)
import Core.SymbolTree
import Data.Foldable

import Prettyprinter

data PrepKind
  = V_I
  | TritoneSub_I
  | Backdoor_I -- dominant - tonic
  | IV_I -- plagal
  | IV_V
  | VI_V -- predominant - dominant
  | Descending5th
  | VOct_I
  | IVOct_I
  deriving (Show, Eq, Generic, Ord)

instance Pretty PrepKind where
  pretty V_I = "V-I"
  pretty TritoneSub_I = "TritoneSub-I"
  pretty Backdoor_I = "Backdoor-I"
  pretty IV_I = "Ⅳ-I"
  pretty IV_V = "Ⅳ-V"
  pretty VI_V = "Ⅵ-V"
  pretty Descending5th = "↘5th"
  pretty VOct_I = "V(Oct)-I"
  pretty IVOct_I = "IV(Oct)-I"

instance ToJSON PrepKind

instance FromJSON PrepKind
instance ToJSONKey PrepKind

data ProlKind
  = Repeat -- same chord
  | RelativeTonic -- tonic of relative key
  | Octatonic
  | Hexatonic -- same tonefeld class
  | ParallelMotion
  deriving (Show, Eq, Generic, Ord)

instance Pretty ProlKind where
  pretty Repeat = "Repeat"
  pretty RelativeTonic = "RelativeTonic"
  pretty Octatonic = "Octatonic"
  pretty Hexatonic = "Hexatonic"
  pretty ParallelMotion = "ParallelMotion"

-- pretty BassUpHalfStep = "BassUpHalfStep"

instance ToJSON ProlKind

instance FromJSON ProlKind
instance ToJSONKey ProlKind

data RuleNames = Prol ProlKind | Prep PrepKind | Term
  deriving (Show, Eq, Generic, Ord)

instance ToJSON RuleNames

instance FromJSON RuleNames
instance ToJSONKey RuleNames

instance Pretty RuleNames where
  pretty (Prol k) = "〖" <> pretty k <> "〗"
  pretty (Prep k) = pretty k
  pretty Term = "*"

data AbstractRule = AbstractRule RuleNames deriving (Eq, Generic)

instance ToJSON AbstractRule

instance FromJSON AbstractRule

instance Show AbstractRule where
  show (AbstractRule (Prol _)) = "Prol"
  show (AbstractRule (Prep _)) = "Prep"
  show (AbstractRule Term) = "Term"

(=~) :: ChordLabel -> ChordLabel -> Bool
(ChordLabel r q _) =~ (ChordLabel r' q' _) = (r, q) == (r', q')

infix 4 =~

(~~) :: ChordLabel -> ChordLabel -> Bool
(ChordLabel r _ _) ~~ (ChordLabel r' _ _) = toMidi (r `pto` r') `mod` 3 == 0

infix 4 ~~

hexatonicEqvi :: ChordLabel -> ChordLabel -> Bool
hexatonicEqvi = (<~>)

(<~>) :: ChordLabel -> ChordLabel -> Bool
(ChordLabel r _ _) <~> (ChordLabel r' _ _) = toMidi (r `pto` r') `mod` 4 == 0

infix 4 <~>

-- >>> toMidi (e' flt `pto` c' nat) `mod` 3
-- 0

relMinorOf :: ChordLabel -> ChordLabel -> Bool
(ChordLabel t q _) `relMinorOf` (ChordLabel t' q' _)
  | (t `pto` t' == minor third') && isMinor q && isMajor q' = True
  | otherwise = False

isMinor :: Quality -> Bool
isMinor (Min, Maj, _) = True
isMinor _ = False

isMajor :: Quality -> Bool
isMajor (Maj, Min, _) = True
isMajor _ = False

-- >>> toMidi (c' shp `pfrom`  d' nat)
-- 11

semitonesAbove :: SPC -> SPC -> Int
semitonesAbove x y = toMidi (x `pfrom` y)

satisfyRule :: RuleNames -> (ChordLabel, ChordLabel) -> ChordLabel -> Bool
satisfyRule r (x, y) z = case r of
  Prol Repeat -> (x =~ z) && (y =~ z)
  Prol RelativeTonic ->
    or
      [ (y == z) && (x `relMinorOf` y)
      , (x == z) && (y `relMinorOf` x)
      , (x == z) && (x `relMinorOf` y)
      , (y == z) && (y `relMinorOf` x)
      ]
  Prep V_I ->
    or
      [ and
          [ y == z
          , root x `semitonesAbove` root y == 7
          , quality x == dom7
          ]
      , and
          [ y == z
          , (root x `semitonesAbove` root y) `elem` [11, 2, 5, 8]
          , quality x `elem` [m7b5, o7]
          ]
      ]
  Prep TritoneSub_I ->
    and
      [ y == z
      , root x `semitonesAbove` root y == 1
      , quality x == dom7
      ]
  Prep Backdoor_I ->
    and
      [ y == z
      , root y `semitonesAbove` root x == 2
      , quality x == dom7
      ]
  Prep VOct_I ->
    and
      [ y == z
      , root x `semitonesAbove` root y `elem` [1, 4, 7, 10]
      ]
  Prep Descending5th ->
    and
      [ y == z
      , root x `semitonesAbove` root y == 7
      , quality x /= dom7
      ]
  Prep IV_I ->
    and
      [ y == z
      , root x `semitonesAbove` root y == 5
      , quality x `notElem` [m7b5, o7] -- avoid overlap with V_I
      ]
  Prep IVOct_I ->
    and
      [ y == z
      , root x `semitonesAbove` root y `elem` [2, 5, 8, 11]
      ]
  Prep IV_V ->
    and
      [ y == z
      , quality y == dom7
      , root y `semitonesAbove` root x == 2
      ]
  Prep VI_V ->
    and
      [ y == z
      , quality y == dom7
      , or
          [ (root x `semitonesAbove` root y == 2) && (quality x == min7) -- could include min triad
          , (root x `semitonesAbove` root y == 1) && (quality x == maj7) -- could include maj triad
          ]
      ]
  Prol Octatonic -> (x `octEqviDiminished` z) && (y `octEqviDiminished` z)
  Prol Hexatonic -> (x `hexatonicEqvi` z) && (y `hexatonicEqvi` z)
  Prol ParallelMotion ->
    and
      [ x == z
      , quality x == quality y
      , or
          [ root x `semitonesAbove` root y `elem` [1, 2]
          , root y `semitonesAbove` root x `elem` [1, 2]
          ]
      ]
  Term -> False

interpreteDiminished :: ChordLabel -> Maybe ChordLabel
interpreteDiminished x =
  if quality x `elem` [m7b5, o7]
    then Just $ x{root = root x -^ major third'}
    else Nothing

octEqviDiminished :: ChordLabel -> ChordLabel -> Bool
octEqviDiminished x y =
  (x ~~ y)
    || ( case interpreteDiminished x of
          Just xDom -> xDom ~~ y
          Nothing -> False
       )

inferRule :: ChordLabel -> [Symbol ChordLabel t] -> Maybe RuleNames
inferRule nt [T _] = Just Term
inferRule nt syms = do
  [x, y] <- mapM extractNT syms
  find
    (\rule -> satisfyRule rule (x, y) nt)
    [ Prol Repeat
    , Prol RelativeTonic
    , Prep IV_I
    , Prep V_I
    , Prep TritoneSub_I
    , Prep Descending5th
    , Prep IV_V
    , Prep VI_V
    , Prep Backdoor_I
    , Prep VOct_I
    , Prol Octatonic
    , Prol Hexatonic
    , Prep IVOct_I
    , Prol ParallelMotion
    ]

-- -- | proof tree with detailed rules
-- annotatedProofTree :: Tree ChordLabel -> Tree (String, String)
-- annotatedProofTree t = (display *** show) <$> inferRuleTree' t

-- -- | proof tree with abstract rules
-- annotatedProofTree' :: Tree ChordLabel -> Tree (String, String)
-- annotatedProofTree' t = zipTree (display <$> t) (show . AbstractRule <$> inferRuleTree t)

-- writeAllParseTree :: FilePath -> FilePath -> IO ()
-- writeAllParseTree dataPath outPath = do
--   ps <- parsePieces dataPath
--   encodeFile (outPath <> "/allProofTrees.json") (pieceParseTree <$> ps)
--   plotAllProofTree ps $ outPath

-- reportProofAllProofTree :: IO ()
-- reportProofAllProofTree = writeAllParseTree
--     "Experiment/DataSet/treebank.json"
--     "Experiment/DataSet/ProofTrees"

-- plotSimpleRulePatternReport = do
--     ps <- pieces
--     let tRule p =   fmap show . inferRuleTree <$> (treeChordLabel . chordTree)  p
--         g0 = treeGrammarCorpus . take 150 $ (mapMaybe tRule ps)
--         compressionProcess = runCompression g0
--         g@(TreeGrammar ps' t) = last compressionProcess

--     encodeFile "(SimpleRule) UnCompressedTreeGrammar.json" (toJSON g0)
--     encodeFile "(SimpleRule) CompressedTreeGrammar.json" (toJSON g)
--     let pds = productions g
--     case Just g of
--         Just g -> renderSVG "(SimpleRule) PatternReport.svg" (dims2D 1000 1000) $ plotDigramStatistics ps' (digramStatistic' g ) -- renderSVG "(SimpleRule) DependencyAnalysis.svg" (dims2D 10000 10000)  (plotDependency g)
--         Nothing -> pure ()
