module Preprocessing.Rhythm.Classical.TreeBankLoader where
import Data.Functor.Foldable.TH
import Core.SymbolTree
import Grammar.Rhythm.RhythmGrammar
import GHC.Generics
import Data.Aeson
import Data.Aeson.Types
import Data.Tree
import Diagrams hiding (Result)
import Visualization.BackEnd
import qualified Data.Vector as Vector
import Control.Applicative
import Prettyprinter
import Data.Foldable (Foldable (toList))
import Data.Functor.Foldable
import Data.Ratio
import Data.Text (splitOn, unpack)
import qualified Data.Text as Text

-- import qualified Data.Vector as Vector

import qualified Diagrams as Diagram
import Diagrams.Prelude (white)

import Text.Read (readMaybe)
import Visualization.Text
import Visualization.Tree
import Core.ParseTree
import Preprocessing.Preprocess hiding (Piece)

data RhythmTree
    = RhythmLeaf String Rational
    | RhythmNode Rational Rational Rational [RhythmTree]
    deriving (Show)

makeBaseFunctor ''RhythmTree

toSymbolTree :: RhythmTree -> SymbolTree RhythmNT RhythmTerminal
toSymbolTree (RhythmLeaf _ d) = TLeaf $ RhythmTerminal SomePitch d
toSymbolTree (RhythmNode a b c ts) =
    NTNode
        (RhythmNT a b c)
        (toSymbolTree <$> ts)

{- | One common encoding mistake in the dataset is to treat terminal as non-terminals. This error manifests as the tree form where a terminal has siblings. (On the contrary, a legal termination rule only produce a single terminal).

Solution: Whenever encounter non-unary branches that contain terminals , rewrite all these terminal leafs as

    @
    Leaf t -> (Node nt [Leaf t])
    where
        nt = [0:dur t: 0]
    @
-}
correctSymbolTree :: SymbolTree RhythmNT RhythmTerminal -> SymbolTree RhythmNT RhythmTerminal
correctSymbolTree (TLeaf x) = TLeaf x
correctSymbolTree (NTNode nt ts) =
    NTNode nt $
        if length ts > 1
            then whenIsLeaf addNT <$> ts
            else correctSymbolTree <$> ts
  where
    f `whenIsLeaf` t@(TLeaf{}) = f t
    _ `whenIsLeaf` t = correctSymbolTree t

    addNT (TLeaf x) = NTNode (RhythmNT 0 (duration x) 0) [TLeaf x]

instance FromJSON RhythmTree where
    parseJSON (Object ob) = do
        sym <- ob .: "label"
        Array xs <- ob .: "children"
        if null xs
            then do
                (pitch, dur) <- parseRhythmT sym
                return $ RhythmLeaf pitch dur
            else do
                (x, y, z) <- parseRhythmNT sym
                ts <- Vector.toList <$> mapM parseJSON xs
                return $ RhythmNode x y z ts
    parseJSON _ = empty

data Piece = Piece {pieceId :: String, pieceTree :: RhythmTree}
    deriving (Show, Generic)
instance FromJSON Piece where
    parseJSON = withObject "Piece" $ \v ->
        Piece
            <$> v .: "id"
            <*> v .: "tree"

data MyTree a = MyTree {label :: a, children :: [MyTree a]}
    deriving (Generic, Show)
instance FromJSON (MyTree String)

rhythmTreeToTree :: RhythmTree -> Tree _
rhythmTreeToTree = cata $ \case
    RhythmLeafF x dur -> Node (pretty x <+> pretty dur) []
    RhythmNodeF a b c ts ->
        Node
            (Prettyprinter.hsep ["[", pretty a, ":", pretty b, ":", pretty c, "]"])
            ts

parseInteger :: Value -> Parser Integer
parseInteger (String x) = do
    let mN = readMaybe $ unpack x
    case mN of
        Just n -> parseJSON (Number n)
        Nothing -> empty -- error $ printf "read fail on %s" (show $ unpack x)
parseInteger _ = empty

parseRational :: Value -> Parser Rational
parseRational (String x) = case splitOn "/" x of
    [n] -> do
        nInt <- parseInteger (String n)
        return $ toRational nInt
    [p, q] -> do
        pInt <- parseInteger (String p)
        qInt <- parseInteger (String q)
        return $ pInt % qInt
    _ -> empty

-- >>> parse parseRational "2/31"
-- Success (2 % 31)

parseRhythmT :: Value -> Parser (String, Rational)
parseRhythmT (String x) = do
    let xs = fmap String . splitOn ":" $ x
    case xs of
        [a, b] -> do
            let (String pitch) = a
            dur <- parseRational b
            return (unpack pitch, dur)
        _ -> empty

parseRhythmNT :: Value -> Parser (Rational, Rational, Rational)
parseRhythmNT (String x) = do
    let xs = fmap String . splitOn ":" . removebracket $ x
    rs <- mapM parseRational xs
    case rs of
        [a, b, c] -> return (a, b, c)
        _ -> empty
  where
    removebracket = Text.init . Text.tail
-- unpack3 [a,b,c] = (a,b,c)
-- unpack3 xs = error $ printf "unpacking elements is not 3: %s from %s"
--     (show xs) (show x)
parseRhythmNT _ = empty

-- >>> parse parseRhythmNT "[0:1/4:-1/8]"
-- Success (0 % 1,1 % 4,(-1) % 8)



parseRhythmTree :: _ -> Maybe RhythmTree
parseRhythmTree = decode

-- >>> parseRhythmTree "{\"label\":\"2%3\",\"children\":[]}"
-- Nothing

-- >>> parseRhythmTree "{\"label\":\"[0:1/8:0]\",\"children\":[{\"label\":\"F3:2/3\",\"children\":[]},{\"label\":\"G5:6/3\",\"children\":[]}]}"
-- Just (RhythmNode (0 % 1) (1 % 8) (0 % 1) [RhythmLeaf "F3" (2 % 3),RhythmLeaf "G5" (2 % 1)])

load :: FilePath -> IO [Piece]
load path = do
    Just (xs :: Array) <- decodeFileStrict path
    let results = fmap (parse (parseJSON @Piece)) (toList xs)
    return $ [x | Success x <- results]

drawRhythmTree :: RhythmTree -> Diagram BackEnd
drawRhythmTree t = Diagram.bg white $ treeDiagram Prelude.id $ drawText . show <$> rhythmTreeToTree t


getParseTree :: Piece -> ParseTree (Maybe RhythmRule) RhythmNT RhythmTerminal
getParseTree =
    inferRuleTree inferRule
        . correctSymbolTree
        . toSymbolTree
        . pieceTree

