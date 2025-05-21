module Preprocessing.Main where
import qualified Preprocessing.JazzHarmony.Main as JazzHarmony
import qualified Preprocessing.Rhythm.Classical.Main as ClassicalRhythm

main :: IO ()
main = JazzHarmony.main >> ClassicalRhythm.main

-- >>>  main
