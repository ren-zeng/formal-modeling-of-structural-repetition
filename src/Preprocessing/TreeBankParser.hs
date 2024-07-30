{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-unticked-promoted-constructors,FlexibleContexts #-}
{-# OPTIONS_GHC -Wno-unused-do-bind #-}

module Preprocessing.TreeBankParser where

import Control.Applicative
import Data.Aeson
import Data.Foldable (Foldable (toList))
import Data.Functor (($>))
import Data.List
import Data.Maybe (mapMaybe)
import Data.Text (Text, justifyRight, pack, toUpper, unpack)
import Data.Tree (flatten)
import Data.Tree qualified
import Generics.SOP qualified
import GHC.Generics (Generic)
import Musicology.Pitch
import Text.Megaparsec (MonadParsec (try), choice, parseMaybe, satisfy, takeRest, Parsec)
import Text.Printf (printf)
import Preprocessing.MusicTheory
import Data.Void

-- import Data.Tree

data Piece = Piece
  { title :: String,
    chords :: [String],
    measures :: [Int],
    beats :: [Int],
    -- turnaround :: Int,
    trees :: [TreeAnalysis],
    -- comments :: String,
    composers :: String,
    year :: Int,
    meter :: Meter,
    key :: String
  }
  deriving (Generic, Show)

instance ToJSON Piece

instance FromJSON Piece

data Meter = Meter
  {numerator :: Int, denominator :: Int}
  deriving (Generic, Show)

instance FromJSON Meter

instance ToJSON Meter

data TreeAnalysis = TreeAnalysis
  { open_constituent_tree :: MyTree String,
    complete_constituent_tree :: MyTree String
  }
  deriving (Generic, Show)

instance FromJSON TreeAnalysis

instance ToJSON TreeAnalysis

data MyTree a = MyTree
  { label :: a,
    children :: [MyTree a]
  }
  deriving (Generic, Show, Functor)

instance FromJSON (MyTree String)

instance ToJSON (MyTree String)

newtype MyList a = MyList [a] deriving (Generic)

instance (FromJSON a) => FromJSON (MyList a) where
  parseJSON = undefined

data BinTree a where
  Leaf :: a -> BinTree a
  Node :: a -> BinTree a -> BinTree a -> BinTree a
  deriving (Show, Generic)

instance Generics.SOP.Generic (BinTree a)

instance Generics.SOP.HasDatatypeInfo (BinTree a)


class ToTree a where 
  toTree' :: a -> Data.Tree.Tree String

bracketShow :: MyTree String -> String
bracketShow (MyTree x []) = x
bracketShow (MyTree _ l@(y1 : y2 : _)) = "[" ++ (intercalate " " (bracketShow <$> l)) ++ "]"
bracketShow (MyTree _ l@[y]) = (intercalate " " (bracketShow <$> l))

toBinTree :: MyTree a -> Maybe (BinTree a)
toBinTree = \case
  MyTree x [] -> Just $ Leaf x
  MyTree x [c1, c2] ->
    do
      t1 <- toBinTree c1
      t2 <- toBinTree c2
      pure (Node x t1 t2)
  _ -> Nothing

instance ToTree (MyTree String) where
  toTree' (MyTree x c) = Data.Tree.Node x (toTree' <$> c)

decodeTreeBank :: FilePath -> IO (Maybe [Result Piece])
decodeTreeBank fName = fmap decodeArray <$> decodeFileStrict @Value fName

decodeArray :: Value -> [Result Piece]
decodeArray (Array v) = toList (fromJSON @Piece <$> v)
decodeArray _ = []

takeSuccess :: [Result a] -> [a]
takeSuccess l = [x | Success x <- l]

filterByName :: String -> Piece -> Bool
filterByName t x = title x == t

filteredTreeBank :: (Piece -> Bool) -> FilePath -> IO [MyTree String]
filteredTreeBank f fName = do
  Just results <- decodeTreeBank fName
  pure $
    mapMaybe
      (pure . complete_constituent_tree)
      (foldMap trees $ filter f (takeSuccess results))

filteredTreeBank' :: (Piece -> Bool) -> FilePath -> IO [Piece]
filteredTreeBank' f fName = do
  Just results <- decodeTreeBank fName
  pure $ filter f (takeSuccess results)

parsePieces :: FilePath -> IO [Piece]
parsePieces file = filteredTreeBank' (const True) file

-- plotTree :: Piece -> IO ()
-- plotTree x = do
--   writeSVG (printf "src/Repetition/TreeBankPlots/%s.svg" name) d
--   putStrLn (printf "drawing %d derivation tree for %s" n name)
--   where
--     d = (hsep 1 . sameBoundingRect) (toTreeDiagram . complete_constituent_tree <$> trees x)
--     n = length (trees x)
--     name = title x

-- plotAllTree :: IO ()
-- plotAllTree = do
--   l <- filteredTreeBank' (const True) "src/Repetition/treebank.json"
--   foldMap plotTree l

-- >>> plotAllTree

toBracketString :: MyTree String -> String
toBracketString x = substitute <$> bracketShow x
  where
    substitute 'b' = '♭'
    substitute '#' = '♯'
    substitute cha = cha

anyChar (chars :: [_]) = satisfy (`elem` chars)

type MyParser a = Parsec Void Text a

pSharp :: MyParser SIC
pSharp = do
  anyChar "#♯"
  pure $ aug unison

pFlat :: MyParser SIC
pFlat = do
  anyChar "-♭b"
  pure $ dim unison

pNat :: MyParser SIC
pNat = do
  many "⛳︎"
  pure unison

pLetterMode :: MyParser (SPC, Mode)
pLetterMode =
  try
    ( do
        k' <- anyChar "ABCDEFG"
        let Just k = readNotationT @SPC $ pack [k']
        pure (k, Major)
    )
    <|> try
      ( do
          k' <- anyChar "abcdefg"
          let Just k = readNotationT @SPC $ toUpper (pack [k'])
          pure (k, Minor)
      )

pKey :: MyParser (SPC, Mode)
pKey = do
  (k, m) <- pLetterMode
  acc <- pSharp <|> pFlat <|> pNat
  pure (k +^ acc, m)


treeChordLabel :: Data.Tree.Tree Text -> Maybe (Data.Tree.Tree ChordLabel)
treeChordLabel (ts :: Data.Tree.Tree _) = mapM (parseMaybe pChordLabel) ts

pChordLabel :: MyParser ChordLabel
pChordLabel = do
  (p, _) <- pKey
  -- q <- pChordQuality
  -- e <- pExtension
  (q, e) <- pChordQualityExtension
  pure (ChordLabel p q e)


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

-- remember to consider order ("m7" before "m") to avoid backtracking
pChordQualityExtension :: MyParser (Quality, Extension)
pChordQualityExtension =
  choice
    [ "^7" $> (maj7, "7"),
      "^" $> (maj7, ""),
      "6" $> (maj7, "6"),
      "m7" $> (min7, "7"),
      "m6" $> (min7, "6"),
      "m^7" $> (min7, "^7"),
      "m" $> (min7, ""),
      "%7" $> (m7b5, "7"),
      "o7" $> (o7, "7"),
      "7" $> (dom7, "7"),
      "sus" $> (dom7, "sus")
    ]

-- >>> parseMaybe pChordQualityExtension "m6"
-- Just ((Min,Maj,Min),"6")

pChordQuality :: MyParser Quality
pChordQuality = choice [pMaj7, pMin7, pFullyDim7, pHalfDim7, pDom7]

pMaj7 :: MyParser Quality
pMaj7 = do
  anyChar "^" <|> anyChar "6"
  pure (Maj, Min, Maj)

pMin7 :: MyParser Quality
pMin7 = do
  anyChar "m"
  pure (Min, Maj, Min)

pDom7 :: MyParser Quality
pDom7 = do
  many "⛳︎" -- a hack to parse empty char
  pure (Maj, Min, Min)

pFullyDim7 :: MyParser Quality
pFullyDim7 = do
  anyChar "o"
  pure (Min, Min, Min)

pHalfDim7 :: MyParser Quality
pHalfDim7 = do
  anyChar "%"
  pure (Min, Min, Maj)

pExtension :: MyParser Extension
pExtension = do
  ex <- takeRest
  pure ex

-- | checking whether the implementation of the pChordLabel can cover all the cases in the JHT corpus, the output is a list of failure cases.
checkAllChordParsable :: FilePath -> IO [(Text, Maybe String)]
checkAllChordParsable file = do
  ps <- parsePieces file
  let allChordText ps' = nub $ (flatten . chordTree) =<< ps'
  let parsed ps' = parseMaybe pChordLabel <$> allChordText ps'
  pure $ zip (allChordText ps) (fmap display <$> parsed ps)


chordLabelInC :: SPC -> ChordLabel -> ChordLabel
chordLabelInC t (ChordLabel p x y) = ChordLabel (transposeToC t p) x y

-- | given a global tonic tranpose a spc to be in the key of C.
-- Eb,G -> E since interval Eb-G = C-E
transposeToC :: Pitch SIC -> Pitch SIC -> Pitch SIC
transposeToC t p = p +^ (t `pto` c' nat)

pieceByName :: String -> [Piece] -> Piece
pieceByName x = head . filter (filterByName x)

chordTree :: Piece -> Data.Tree.Tree Text
chordTree = fmap pack . toTree' . complete_constituent_tree . head . trees

chordSequence :: Piece -> Text
chordSequence = pack . toBracketString . complete_constituent_tree . head . trees

getKeyMode :: Piece -> Maybe (SPC, Mode)
getKeyMode p = parseMaybe pKey (pack $ key p)

getChordTree :: Piece -> Data.Tree.Tree String
getChordTree = toTree' . complete_constituent_tree . head . trees

keyOfPiece :: Piece -> Maybe (SPC, Mode)
keyOfPiece = parseMaybe pKey . pack . key

getTransposedChordTree :: Piece -> Maybe (Data.Tree.Tree ChordLabel)
getTransposedChordTree p = case keyOfPiece p of
  Nothing -> Nothing
  Just (t, m) -> fmap (chordLabelInC t) <$> x
  where
    x = (treeChordLabel . fmap pack . getChordTree) p
