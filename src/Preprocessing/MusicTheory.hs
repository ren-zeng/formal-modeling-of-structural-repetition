module Preprocessing.MusicTheory where 

import Musicology.Pitch (SPC)
import Data.Text
import Text.Printf
import GHC.Generics
import Data.Aeson
import Prettyprinter

data Mode = Major | Minor deriving (Show, Eq)
data Key = Key SPC Mode deriving (Show)


data ThirdQuality = Maj | Min deriving (Show, Eq,Generic)
instance ToJSON ThirdQuality
instance FromJSON ThirdQuality


type Extension = Text

-- | the quality of a seventh chord is a triplet of the qualities of thirds
-- Major Seventh is defined as (M3,m3,M3) so we just encode (Maj, Min, Maj)
type Quality = (ThirdQuality, ThirdQuality, ThirdQuality)

data ChordLabel = ChordLabel {root :: SPC, quality :: Quality, extension :: Extension} deriving (Show, Eq,Generic)

instance ToJSON ChordLabel
instance FromJSON ChordLabel

instance Pretty ChordLabel where
  pretty = pretty . display

display :: ChordLabel -> String
display (ChordLabel t q e) = printf "%s%s%s" (show t) (dq :: String) (de :: Extension)
  where
    (dq, de) = go (q, e)
    go x
      | x == (maj7, "7") = ("^", "7")
      | x == (maj7, "") = ("^", "")
      | x == (maj7, "6") = ("", "6")
      | x == (min7, "7") = ("m", "7")
      | x == (min7, "6") = ("m", "6")
      | x == (min7, "^7") = ("m", "^7")
      | x == (min7, "") = ("m", "")
      | x == (m7b5, "7") = ("%", "7")
      | x == (o7, "7") = ("o", "7")
      | x == (dom7, "7") = ("", "7")
      | x == (dom7, "sus") = ("", "sus")
      | otherwise = ("?", "?")

dom7 :: Quality
dom7 = (Maj, Min, Min)

maj7 :: Quality
maj7 = (Maj, Min, Maj)

min7 :: Quality
min7 = (Min, Maj, Min)

m7b5 :: Quality
m7b5 = (Min, Min, Maj)

o7 :: Quality
o7 = (Min, Min, Min)

majorC :: ChordLabel -> Bool
majorC (ChordLabel _ q _) = majorQ q

minorC :: ChordLabel -> Bool
minorC (ChordLabel _ q _) = minorQ q

majorQ :: Quality -> Bool
majorQ (Maj, Min, _) = True
majorQ _ = False

minorQ :: Quality -> Bool
minorQ (Min, Maj, _) = True
minorQ _ = False