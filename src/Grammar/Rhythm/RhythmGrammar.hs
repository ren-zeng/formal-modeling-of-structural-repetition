{-# LANGUAGE MultiWayIf #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use &&" #-}
{-# HLINT ignore "Use ||" #-}

module Grammar.Rhythm.RhythmGrammar where

import Control.Applicative
import Control.Monad
import Data.Foldable
import Data.Functor.Foldable
import Data.Ratio (denominator, numerator, (%))
import Data.Tree

import Core.ParseTree
import Core.SymbolTree
import Data.Aeson
import GHC.Generics (Generic)
import Prettyprinter


data Pitch = SomePitch deriving (Show, Eq, Ord, Generic)

instance ToJSON Pitch
instance FromJSON Pitch

type Duration = Rational

data RhythmTerminal = RhythmTerminal {pitch :: Pitch, duration :: Duration}
    deriving (Show, Eq, Ord, Generic)

instance ToJSON RhythmTerminal
instance FromJSON RhythmTerminal

instance Pretty RhythmTerminal where
    pretty x = pretty $ duration x

data RhythmNT = RhythmNT
    { upbeat :: Duration
    , body :: Duration
    , coda :: Duration
    }
    deriving (Show, Eq, Ord, Generic)

instance ToJSON RhythmNT
instance FromJSON RhythmNT

instance Pretty Duration where
    pretty x =
        if
            | p == 0 || q == 1 -> pretty p
            | otherwise -> pretty p <> "/" <> pretty q
      where
        (p, q) = (numerator x, denominator x)

instance Pretty RhythmNT where
    pretty (RhythmNT a b c) ="[" <> pretty a <> ":" <> pretty b <> ":" <> pretty c <> "]"

data TemporalDirection = Early | Late
    deriving (Show, Eq, Ord, Generic)

instance ToJSON TemporalDirection
instance FromJSON TemporalDirection

{- |
- @Split@

    @
    [a:b:c] -> [a:b₁:c₁] [a₂:b₂:c]
    where
        b₁ + b₂ = b
        c₁ + a₂ = 0
    @

- @Prepare@

    @
    [a:b:c] -> [a₁:b₁:c₁] [a₂:b:c]
    where
        a₁ + b₁ + c₁ + a₂ = a
        
    @

- @Shift Early (Anticipation)@

    @
    [a:b:c] -> [0:b:c₁]
    where
        a₁ + b₁ = a
        c₁ = a + c
    @
-}
data RhythmRule
    = Split
    | Respell
    | Prepare
    | Shift TemporalDirection
    | Terminate
    deriving
        ( -- | AnythingGoes
          Show
        , Eq
        , Ord
        , Generic
        )

instance ToJSON RhythmRule
instance FromJSON RhythmRule

instance ToJSONKey RhythmRule


instance Pretty RhythmRule where
    pretty = \case 
        Split -> "Split"
        Respell -> "Respell"
        Prepare -> "Prepare"
        Shift Early -> "Shift Early"
        Shift Late -> "Shift Late"
        Terminate -> "*"

satisfyRule :: RhythmRule -> RhythmNT -> [RhythmNT] -> Bool
satisfyRule Split x [x1, x2] =
    and
        [ upbeat x1 == upbeat x
        , coda x2 == coda x
        , body x1 + body x2 == body x
        , coda x1 + upbeat x2 == 0
        ]
satisfyRule Respell x [x1] =
    or
        [ and
            [ coda x1 == coda x
            , upbeat x1 + body x1 == upbeat x + body x
            ]
        , and
            [ upbeat x1 == upbeat x
            , body x1 + coda x1 == body x + coda x
            ]
        ]
satisfyRule Split x [x1, x2, x3] =
    and
        [ upbeat x1 == upbeat x
        , coda x3 == coda x
        , body x1 + body x2 + body x3 == body x
        , coda x1 + upbeat x2 == 0
        , coda x2 + upbeat x3 == 0
        ]
satisfyRule Prepare x [x1, x2] =
    and
        [ body x2 == body x
        , coda x2 == coda x
        , upbeat x1 + body x1 + coda x1 + upbeat x2 == upbeat x
        ]
satisfyRule (Shift Early) x [x1] =
    and
        [ body x1 == body x
        , upbeat x1 + coda x1 == upbeat x + coda x
        , upbeat x1 < upbeat x
        ]
satisfyRule (Shift Late) x [x1] =
    and
        [ body x1 == body x
        , upbeat x1 + coda x1 == upbeat x + coda x
        , coda x1 > coda x
        ]
satisfyRule _ _ _ = False

testRule =
    inferRule
        (RhythmNT (5 % 16) (1 % 2) (-3 % 8))
        [NT $ RhythmNT 0 (1 % 16) 0, NT $ RhythmNT (1 % 4) (1 % 2) (-3 % 8)]

-- >>> testRule
-- Just Prepare

inferRule :: RhythmNT -> [Symbol RhythmNT t] -> Maybe RhythmRule
inferRule nt = \case
    [T _] -> return Terminate
    rhs -> do
        nts <- mapM extractNT rhs -- making sure all symbols are NT
        find
            (\r -> satisfyRule r nt nts)
            [ Split
            , Prepare
            , Shift Early
            , Shift Late
            , Respell
            ]

-- \case
-- NTNode nt [TLeaf t] -> return $ ParseTree nt Terminate [Leaf t]
-- NTNode nt ts -> do
--     case mapM rootNT ts of
--         Just xs -> do
--             r <- find (\r -> satisfyRule r nt xs) ntRules
--             ts' <- mapM inferRuleTree ts
--             return $ ParseTree nt r ts'
--         Nothing -> error $ show $ "inferRuleTree encounters a NTNode whose children are not all NT" <+> viaShow (rootNT <$> ts)
-- TLeaf x -> error "impossible case in inferRuleTree"
data RuleCategory = Splitting | Preparing | Respelling | Shifting | Terminating
    deriving (Show, Eq, Ord, Generic)
instance ToJSON RuleCategory
instance FromJSON RuleCategory

ruleCategory :: RhythmRule -> RuleCategory
ruleCategory = \case
    Split -> Splitting
    Respell -> Respelling
    Prepare -> Preparing
    Shift _ -> Shifting
    Terminate -> Terminating
