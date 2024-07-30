module Compression.Meta where


import RIO.List
import Data.Aeson
import GHC.Generics hiding (Meta)


data RepSymbol = New | Star | RepLoc Int deriving (Eq, Ord, Generic)
instance ToJSON RepSymbol
instance FromJSON RepSymbol

instance Show RepSymbol where
  show New = "_"
  show Star = "★"
  show (RepLoc n) = show n
type Meta = [RepSymbol]

useMeta' :: Meta -> a -> [a] -> [a] -> [a]
useMeta' [] _ _ done = done
useMeta' (m : ms) r xs done = case m of
  New -> useMeta' ms r (tail xs) (done ++ [head xs])
  Star -> useMeta' ms r xs (done ++ [r])
  RepLoc i -> useMeta' ms r xs (done ++ [done !! i])

useMeta :: Meta -> a -> [a] -> [a]
useMeta ms r xs = useMeta' ms r xs []

reduceByMeta :: Meta -> [a] -> [a]
reduceByMeta m xs = [x | (x, s) <- zip xs m, s == New]

-- >>> reduceByMeta [Star,New,RepLoc 1, RepLoc 1, New, RepLoc 1] [9,12,12,12,99,12]
-- [12,99]

simplifyByMeta :: Meta -> [a] -> [a]
simplifyByMeta m xs = [x | (s, x) <- zip m xs, s == New]

freeVar :: Meta -> Int
freeVar = foldr (\x -> if x == New then (+ 1) else id) 0

toEqClass :: (Enum b) => b -> Meta -> [b]
toEqClass r m = useMeta m r $ toEnum <$> [1 .. length m]

inferMetaAcc :: (Eq b) => b -> [b] -> [b] -> Meta
inferMetaAcc r seen [] = []
inferMetaAcc r seen (x : xs) = y : inferMetaAcc r (seen ++ [x]) xs
  where
    y = if x == r then Star else maybe New RepLoc (elemIndex x seen)

inferMeta :: (Eq b) => b -> [b] -> Meta
inferMeta r = inferMetaAcc r []

-- >>>toEqClass @Int  0 $  fromEqClass 'T' "aabcT"
-- [1,1,2,3,0]

class MetaLike a where
  useRep :: a -> b -> [b] -> [b]
  freeVar' :: a -> Int
  toEqClass' :: (Enum b) => b -> a -> [b]
  inferMeta' :: (Eq b) => b -> [b] -> a

instance MetaLike Meta where
  useRep = useMeta
  freeVar' = freeVar
  toEqClass' = toEqClass
  inferMeta' = inferMeta 
  
