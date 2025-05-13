module Experiment.Computation where
import GHC.Generics
import Data.Functor.Foldable.TH (MakeBaseFunctor(makeBaseFunctor))
import Data.Functor.Foldable


data Computation r = Hole | Computation r [Computation r]
    deriving (Show, Eq, Generic)

makeBaseFunctor ''Computation


numTs :: Computation r -> Int
numTs = cata $ \case 
    HoleF -> 0 
    ComputationF _ [] -> 1
    ComputationF _ ns -> sum ns

numNTs :: Computation r -> Int
numNTs = cata $ \case 
    HoleF -> 1 
    ComputationF _ ns -> sum ns

computationSize :: Computation r -> Int 
computationSize = cata $ \case 
    HoleF -> 0
    ComputationF _ ns -> 1 + sum ns