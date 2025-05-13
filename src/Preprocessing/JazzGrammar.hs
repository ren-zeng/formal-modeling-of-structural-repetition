{-# LANGUAGE BlockArguments #-}

module Preprocessing.JazzGrammar where

import Control.Arrow
import Data.Aeson
import Data.Tree
import GHC.Generics (Generic)
import Musicology.Pitch
import Preprocessing.MusicTheory
import Preprocessing.TreeBankParser (Piece (title), chordTree, parsePieces, treeChordLabel, addTerminal)

import Control.Monad
import Core.ProofTree (ProofTree)
import Data.Foldable
import Diagrams (mkWidth,bg, frame)
import Diagrams.Backend.SVG (renderSVG)
import Text.Printf
import Visualization.ProofTree
import Visualization.Text
import Core.ParseTree (inferParseTree, ParseTree (..), ParseTreeF (..))
import Core.SymbolTree
import Visualization.ParseTree (drawParseTree)
import Data.Functor.Foldable
import Prettyprinter
import Diagrams.Prelude (white)
import Data.Maybe



(/\) = (&&)

infixr 3 /\

data PrepKind = V_I | IV_I | IV_V | VI_V | Diatonic5th | TritoneSub | Backdoor deriving (Show, Eq, Generic, Ord)

instance Pretty PrepKind where
  pretty V_I = "V-I"
  pretty IV_I = "Ⅳ-I"
  pretty IV_V = "Ⅳ-V"
  pretty VI_V = "Ⅳ-V"
  pretty Diatonic5th = "Diatonic5th"
  pretty TritoneSub = "TritoneSub"
  pretty Backdoor = "Backdoor"

instance ToJSON PrepKind

instance FromJSON PrepKind

data ProlKind = Repeat | VI_I | I_VI | I_III | III_I | Octatonic | Hexatonic deriving (Show, Eq, Generic, Ord)
instance Pretty ProlKind where
  pretty Repeat = "Repeat"
  pretty VI_I = "Ⅵ-I"
  pretty I_VI = "I-Ⅵ"
  pretty I_III = "I-Ⅲ"
  pretty III_I = "Ⅲ-I"
  pretty Octatonic = "Octatonic"
  pretty Hexatonic = "Hexatonic"

instance ToJSON ProlKind

instance FromJSON ProlKind

data RuleNames = Prol ProlKind | Prep PrepKind | Term | Ɂ
  deriving (Show, Eq, Generic, Ord)


instance ToJSON RuleNames

instance FromJSON RuleNames

instance Pretty RuleNames where
  pretty (Prol k) = pretty k
  pretty (Prep k) = pretty k
  pretty Term = "Term"
  pretty Ɂ = "?"

data AbstractRule = AbstractRule RuleNames deriving (Eq, Generic)

instance ToJSON AbstractRule

instance FromJSON AbstractRule

instance Show AbstractRule where
  show (AbstractRule (Prol _)) = "Prol"
  show (AbstractRule (Prep _)) = "Prep"
  show (AbstractRule Term) = "Term"
  show (AbstractRule Ɂ) = "?"

(=~) :: ChordLabel -> ChordLabel -> Bool
(ChordLabel r q _) =~ (ChordLabel r' q' _) = (r, q) == (r', q')

infix 4 =~

(~~) :: ChordLabel -> ChordLabel -> Bool
(ChordLabel r _ _) ~~ (ChordLabel r' _ _) = toMidi (r `pto` r') `mod` 3 == 0

infix 4 ~~

(<~>) :: ChordLabel -> ChordLabel -> Bool
(ChordLabel r _ _) <~> (ChordLabel r' _ _) = toMidi (r `pto` r') `mod` 4 == 0

infix 4 <~>

-- >>> toMidi (e' flt `pto` c' nat) `mod` 3
-- 0

relMinorOf :: ChordLabel -> ChordLabel -> Bool
(ChordLabel t q _) `relMinorOf` (ChordLabel t' q' _)
  | (t `pto` t' == minor third') /\ isMinor q /\ isMajor q' = True
  | otherwise = False

isMinor :: Quality -> Bool
isMinor (Min, Maj, _) = True
isMinor _ = False

isMajor :: Quality -> Bool
isMajor (Maj, Min, _) = True
isMajor _ = False

satisfyRule :: RuleNames -> (ChordLabel, ChordLabel) -> ChordLabel -> Bool
satisfyRule r (x, y) z = case r of
  Prol Repeat -> (x =~ z) /\ (y =~ z)
  Prol VI_I -> (y == z) /\ (x `relMinorOf` y)
  Prol I_VI -> (x == z) /\ (y `relMinorOf` x)
  Prol I_III -> (x == z) /\ (x `relMinorOf` y)
  Prol III_I -> (y == z) /\ (y `relMinorOf` x)
  Prep V_I ->
    (y == z)
      /\ (root y `pto` root x == fifth')
      /\ (quality x == dom7)
  Prep Diatonic5th ->
    (y == z)
      /\ (root y `pto` root x == fifth')
      /\ (quality x /= dom7)
  Prep IV_I ->
    (y == z)
      /\ (root y `pto` root x == fourth')
      /\ (quality x `elem` [maj7, min7, dom7])
  Prep TritoneSub ->
    (y == z)
      /\ (root y `pto` root x `elem` [minor second', aug unison])
  -- (quality x == dom7)
  Prep Backdoor ->
    (y == z)
      /\ (root x `pto` root y == major second')
  -- (quality x == dom7)
  Prep IV_V ->
    (y == z)
      /\ (root x `pto` root y == major second')
      /\ (quality y == dom7)
  Prep VI_V ->
    (y == z)
      /\ ( (root y `pto` root x == major second') /\ (quality x == min7)
            || (root y `pto` root x == minor second') /\ (quality x == maj7)
         )
      /\ (quality y == dom7)
  Prol Octatonic -> (x ~~ z) /\ (y ~~ z)
  Prol Hexatonic -> (x <~> z) /\ (y <~> z)
  Term -> False
  Ɂ -> True

unExplained :: Tree RuleNames -> Bool
unExplained t = Ɂ `elem` t



-- inferRuleTree' :: Tree ChordLabel -> Tree (ChordLabel, RuleNames)
-- inferRuleTree' (Node x []) = Node (x, Term) []
-- inferRuleTree' (Node x [l, r]) =
--   case findRule (rootLabel l, rootLabel r) x rules of
--     Just rule -> mergeWith rule
--     Nothing -> mergeWith Ɂ
--  where
--   rules =
--     [ Prol Repeat
--     , Prol I_VI
--     , Prol VI_I
--     , Prol I_III
--     , Prol III_I
--     , Prep V_I
--     , Prep Diatonic5th
--     , Prep IV_I
--     , Prep IV_V
--     , Prep VI_V
--     , Prep TritoneSub
--     , Prep Backdoor
--     , Prol Octatonic
--     , Prol Hexatonic
--     ]
--   mergeWith k = Node (x, k) (inferRuleTree' <$> [l, r])
--   findRule a b = find (\rule -> satisfyRule rule a b)
-- inferRuleTree' _ = error "encountered non-binary split"

inferRule :: ChordLabel -> [Symbol ChordLabel t] -> Maybe RuleNames
inferRule nt [T _] = Just $ Term  
inferRule nt syms = do 
  [x,y] <- mapM extractNT syms
  find (\rule -> satisfyRule rule (x,y) nt) 
    [ Prol Repeat
      , Prol I_VI
      , Prol VI_I
      , Prol I_III
      , Prol III_I
      , Prep V_I
      , Prep Diatonic5th
      , Prep IV_I
      , Prep IV_V
      , Prep VI_V
      , Prep TritoneSub
      , Prep Backdoor
      , Prol Octatonic
      , Prol Hexatonic
      , Ɂ
      ]





-- -- | proof tree with detailed rules
-- annotatedProofTree :: Tree ChordLabel -> Tree (String, String)
-- annotatedProofTree t = (display *** show) <$> inferRuleTree' t

-- -- | proof tree with abstract rules
-- annotatedProofTree' :: Tree ChordLabel -> Tree (String, String)
-- annotatedProofTree' t = zipTree (display <$> t) (show . AbstractRule <$> inferRuleTree t)

pieceSymbolTree :: Piece -> Maybe (SymbolTree ChordLabel ChordLabel)
pieceSymbolTree  = (return . addTerminal) <=< (treeChordLabel . chordTree)

pieceParseTree :: Piece -> Maybe (ParseTree RuleNames ChordLabel ChordLabel)
pieceParseTree = inferParseTree inferRule <=< pieceSymbolTree


plotAllProofTree :: (Foldable t) => t Piece -> FilePath -> IO ()
plotAllProofTree ps outFolder = do
  forM_ ps $ \p ->
    case pieceParseTree p of 
      Nothing -> pure () 
      Just t -> 
        renderSVG
          (printf "%s/(proof) %s.svg" outFolder (title p))
          (mkWidth 1000)
          (Diagrams.bg white $ drawParseTree (frame 0.25 . drawText .  show . pretty) (drawText . show.pretty) t)


writeAllParseTree :: FilePath -> FilePath -> IO ()
writeAllParseTree dataPath outPath = do
  ps <- parsePieces dataPath
  encodeFile (outPath <> "/allProofTrees.json") (pieceParseTree <$> ps)
  plotAllProofTree ps $ outPath 


reportProofAllProofTree :: IO () 
reportProofAllProofTree = writeAllParseTree
    "Experiment/DataSet/treebank.json"
    "Experiment/DataSet/ProofTrees"



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

parseTreeToRuleTree :: ParseTree r nt t 
  -> Maybe (Tree r)
parseTreeToRuleTree = \case 
  Leaf _ -> Nothing 
  ParseTree _ r ts -> return $ Node r  $ mapMaybe parseTreeToRuleTree ts 

tRule :: Piece -> Maybe (Tree RuleNames)
tRule = parseTreeToRuleTree <=< pieceParseTree 


