{-# LANGUAGE MultiWayIf #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use &&" #-}

module Grammar.Rhythm.RhythmGrammar where

import Control.Applicative
import Control.Monad
import Data.Foldable
import Data.Functor.Foldable
import Data.Ratio ( denominator, numerator )
import Data.Tree

import Core.ParseTree
import Core.SymbolTree
import Prettyprinter
import Data.Aeson
import GHC.Generics (Generic)

data Pitch = SomePitch deriving (Show,Eq,Ord,Generic)

instance ToJSON Pitch 
instance FromJSON Pitch

type Duration = Rational

data RhythmTerminal = RhythmTerminal {pitch :: Pitch, duration :: Duration}
    deriving (Show,Eq,Ord,Generic)

instance ToJSON RhythmTerminal 
instance FromJSON RhythmTerminal

instance Pretty RhythmTerminal where
    pretty x = pretty $ duration x

data RhythmNT = RhythmNT
    { upbeat :: Duration
    , body :: Duration
    , coda :: Duration
    }
    deriving (Show,Eq,Ord,Generic)

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
    pretty (RhythmNT a b c) = hsep ["[", pretty a, ":", pretty b, ":", pretty c, "]"]

data TemporalDirection = Early | Late
    deriving (Show,Eq,Ord,Generic)

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
        a₁ + b₁ = a
        c₁ + a₂ = 0
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
    | Prepare
    | Shift TemporalDirection
    | Terminate
    | AnythingGoes
    deriving (Show,Eq,Ord,Generic)

instance ToJSON RhythmRule 
instance FromJSON RhythmRule

instance Pretty RhythmRule where
    pretty = pretty . show

satisfyRule :: RhythmRule -> RhythmNT -> [RhythmNT] -> Bool
satisfyRule Split x [x1, x2] =
    and
        [ upbeat x1 == upbeat x
        , coda x2 == coda x
        , body x1 + body x2 == body x
        , coda x1 + upbeat x2 == 0
        ]
satisfyRule Prepare x [x1, x2] =
    and
        [ body x2 == body x
        , coda x2 == coda x
        , upbeat x1 + body x1 == upbeat x
        , coda x1 + upbeat x2 == 0
        ]
satisfyRule (Shift Early) x [x1] =
    and
        [ body x1 == body x
        , upbeat x1 == 0
        , coda x1 == upbeat x + coda x
        ]
satisfyRule (Shift Late) x [x1] =
    and
        [ body x1 == body x
        , coda x1 == 0
        , upbeat x1 == upbeat x + coda x
        ]
satisfyRule AnythingGoes _ _ = True
satisfyRule _ _ _ = False

inferRule :: RhythmNT -> [Symbol RhythmNT t] -> Maybe RhythmRule
inferRule nt = \case
    [T _] -> return Terminate
    rhs -> do
        nts <- mapM extractNT rhs -- making sure all symbols are NT
        find
            (\r -> satisfyRule r nt nts)
            [Split, Prepare, AnythingGoes]



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

