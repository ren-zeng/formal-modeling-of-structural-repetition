{-# LANGUAGE BlockArguments #-}

module Preprocessing.JazzGrammar where

import Data.Aeson
import Data.Tree
import GHC.Generics (Generic)
import Preprocessing.MusicTheory
import Musicology.Pitch
import Preprocessing.TreeBankParser (chordTree, treeChordLabel, parsePieces, Piece (title), display)
import Control.Arrow
import Visualization.TreeVisualizer
import Data.Foldable
import Text.Printf

(/\) = (&&)

infixr 3 /\

data PrepKind = V_I | IV_I | IV_V | VI_V | Diatonic5th | TritoneSub | Backdoor deriving (Show, Eq, Generic)

instance ToJSON PrepKind

instance FromJSON PrepKind

data ProlKind = Repeat | VI_I | I_VI | I_III | III_I | Octatonic | Hexatonic deriving (Show, Eq, Generic)

instance ToJSON ProlKind

instance FromJSON ProlKind

data RuleNames = Prol ProlKind | Prep PrepKind | Term | Ɂ deriving (Show, Eq, Generic)

instance ToJSON RuleNames

instance FromJSON RuleNames

data AbstractRule = AbstractRule RuleNames deriving (Eq, Generic)

instance ToJSON AbstractRule

instance FromJSON AbstractRule

instance Show AbstractRule where
  show (AbstractRule (Prol _)) = "Prol"
  show (AbstractRule (Prep _)) = "Prep"
  show (AbstractRule Term) = "Term"
  show (AbstractRule Ɂ) = "Ɂ"

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

inferRuleTree' :: Tree ChordLabel -> Tree (ChordLabel,RuleNames)
inferRuleTree' (Node x []) = Node (x,Term) []
inferRuleTree' (Node x [l, r])
  | satisfyRule (Prol Repeat) (c1, c2) x = mergeWith (Prol Repeat)
  | satisfyRule (Prol I_VI) (c1, c2) x = mergeWith (Prol I_VI)
  | satisfyRule (Prol VI_I) (c1, c2) x = mergeWith (Prol VI_I)
  | satisfyRule (Prol I_III) (c1, c2) x = mergeWith (Prol I_III)
  | satisfyRule (Prol III_I) (c1, c2) x = mergeWith (Prol III_I)
  | satisfyRule (Prep V_I) (c1, c2) x = mergeWith (Prep V_I)
  | satisfyRule (Prep Diatonic5th) (c1, c2) x = mergeWith (Prep Diatonic5th)
  | satisfyRule (Prep IV_I) (c1, c2) x = mergeWith (Prep IV_I)
  | satisfyRule (Prep IV_V) (c1, c2) x = mergeWith (Prep IV_V)
  | satisfyRule (Prep VI_V) (c1, c2) x = mergeWith (Prep VI_V)
  | satisfyRule (Prep TritoneSub) (c1, c2) x = mergeWith (Prep TritoneSub)
  | satisfyRule (Prep Backdoor) (c1, c2) x = mergeWith (Prep Backdoor)
  -- if the previous can't explain, here comes the tonfeld theory rules
  | satisfyRule (Prol Octatonic) (c1, c2) x = mergeWith (Prol Octatonic)
  | satisfyRule (Prol Hexatonic) (c1, c2) x = mergeWith (Prol Hexatonic)
  -- a catch-all rule
  | satisfyRule Ɂ (c1, c2) x = mergeWith Ɂ
  where
    (c1, c2) = (rootLabel l, rootLabel r)
    mergeWith k = Node (x,k) (inferRuleTree' <$> [l, r])
inferRuleTree' _ = error "encountered non-binary split"

inferRuleTree :: Tree ChordLabel -> Tree RuleNames
inferRuleTree = fmap snd . inferRuleTree'

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

-- | proof tree with detailed rules
annotatedProofTree :: Tree ChordLabel -> Tree (String, String)
annotatedProofTree t = (display *** show) <$> inferRuleTree' t

-- -- | proof tree with abstract rules
-- annotatedProofTree' :: Tree ChordLabel -> Tree (String, String)
-- annotatedProofTree' t = zipTree (display <$> t) (show . AbstractRule <$> inferRuleTree t)


plotAllProofTree :: Foldable t => t Piece -> FilePath -> IO ()
plotAllProofTree ps outFolder = do
    let ct  = treeChordLabel . chordTree
        proofTree t = annotatedProofTree <$> (treeChordLabel . chordTree) t
        tRule t =  proofTreeDiagram <$> proofTree t
        unexplainable = fmap (unExplained . inferRuleTree) . (treeChordLabel . chordTree)
        plot x = forM_ (tRule x) (writeSVG (printf "%s/(proof) %s.svg" outFolder (title x)))
    mapM_ plot ps
    -- pure . (\x-> (length x, title <$> x)) $ filter  ((== Just True) . unexplainable ) ps

-- -- >>> plotAllProofTree
-- -- (39,["Beatrice","Lady Bird","Afro Blue","Brotherhood Of Man","Half Nelson","Leaving","Bluesette","Paper Doll","Serenity","Simone","St. Thomas","Night Dreamer","It's Been A Long Long Time","I Wish I Knew How It Would Feel To Be Free","Glad To Be Unhappy","In A Mellow Tone (In A Mellotone)","Move","Someday My Prince Will Come","Cool One, The","I've Got A Crush On You","Jeannie's Song","Boy Next Door","Smile","Struttin' With Some Barbecue","502 Blues","Bill Bailey","Blue Lou","In A Little Spanish Town","My Melancholy Baby","Nuages","Them There Eyes","Zingaro (Retrato Em Branco E Preto)","Dancing In The Dark","Donna Lee","Fine Romance, A","Hackensack","Jersey Bounce","Just In Time","Best Things In Life Are Free, The"])

writeAllProofTree :: FilePath -> IO [Maybe (Tree RuleNames)]
writeAllProofTree file = do
    ps <- parsePieces file
    let f = fmap inferRuleTree . treeChordLabel . chordTree
        r = f <$> ps
    encodeFile "allProofTrees.json" r
    return r

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



tRule :: Piece -> Maybe (Tree RuleNames)
tRule p = do
  ct <- (treeChordLabel . chordTree) p
  let t = inferRuleTree ct
  return t

ruleTree :: FilePath -> IO (Maybe (Tree RuleNames))
ruleTree file = do
  ps <- parsePieces file
  return $ tRule (head ps)
