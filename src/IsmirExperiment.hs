{-# LANGUAGE TupleSections #-}
module IsmirExperiment where
    
import           Data.Maybe


import           Control.Arrow
import           Data.Aeson
import           Data.Function                 (on)
import           Data.List                     (sortBy, sortOn)
import qualified Data.Map                      as Map
import           Data.Ord
import GHC.Generics hiding (Meta)
import Compression.SLFP
import Preprocessing.TreeBankParser
import Compression.Meta
import Preprocessing.JazzGrammar

treebankPath :: String
treebankPath = "experiment/data/treebank.json"

proofTreeFolderPath :: String
proofTreeFolderPath = "experiment/data/ProofTrees"


namedRuleTree p = (title p,) . fmap show <$>  tRule p



corpusSLFP ps = initSLFP $ take 150 (mapMaybe namedRuleTree ps)

pieces = parsePieces treebankPath

-- --compressCorpus :: IO (SLFP String (String, (c, c')))
-- compressCorpus = do
--     ps <- pieces
--     let steps = compressGSteps . corpusSLFP $ ps
--     let compressed = last steps
--     -- writeSVG "JazzCorpus.svg" . plotSeqDiagrams  $ plotSLFP <$> steps
--     --pure $ (localMetas &&& globalMetas) compressed
--     pure $ size <$> steps

-- -- >>> compressCorpus
-- -- [260,206,179,169,161,154,147,142,139,137,135,134,134,134,134,135,136,137,138,139,140,141,142,143,145,146]


minedMetas :: SLFP String b -> [(String, Meta)]
minedMetas = filter (\(_,m) -> length m <= 4) . sortOn ( metaRuleNameToInt .fst) . Map.toList . globalMetas

-- >>> (minedMetas . fixedPoint compressG . corpusSLFP) <$> pieces
-- [("RG1",[_,0]),("RG4",[★,_]),("RG9",[_,_,0,1]),("RG13",[★]),("RG16",[_,★]),("RG17",[_,0,0,0]),("RG20",[_,_,0,_]),("RG27",[_,_,_,1]),("RG62",[_,0,_,0]),("RG63",[_,0,_,_]),("RG64",[_,0,_]),("RG65",[_,_,_,0]),("RG100",[_,_,0,0]),("RG101",[_,_,_,2]),("RG148",[_,_,1,_])]

metaRuleNameToInt :: String -> Int
metaRuleNameToInt  = read . drop 2

-- >>> metaRuleNameToInt "RG115"
-- 115

ruleSummary slfp = (
    sum $ Map.size . patterns  <$> Map.elems (sltps slfp),
    sum $ Map.size . metas     <$> Map.elems (sltps slfp),
    Map.size $ globalPatterns slfp,
    Map.size $ globalMetas slfp)

-- >>> (ruleSummary . fixedPoint compressG . corpusSLFP) <$> pieces
-- (36,0,198,20)



individualPieceChange :: (Ord k) => SLFP a1 k -> SLFP a2 k -> Map.Map k (Int, Int)
individualPieceChange slfpOrig selfFin = Map.intersectionWith (,) (size <$> sltps slfpOrig) (size <$> sltps selfFin)

-- >>> uncurry individualPieceChange . (id &&& fixedPoint compressG) . corpusSLFP <$> pieces 
-- fromList [("502 Blues",(69,20)),("Across The Valley From The Alamo",(73,27)),("Afro Blue",(39,18)),("All Of Me",(61,22)),("Armando's Rhumba",(43,19)),("Autumn Leaves",(67,9)),("Avalon",(63,19)),("Backstage Sally",(57,16)),("Ballade",(49,21)),("Baubles, Bangles And Beads",(71,22)),("Beatrice",(33,13)),("Beautiful Friendship, A",(67,27)),("Beautiful Love",(71,26)),("Bemsha Swing",(57,27)),("Best Things In Life Are Free, The",(73,22)),("Bill Bailey",(65,24)),("Birk's Works",(31,13)),("Blue Bossa",(29,17)),("Blue In Green",(31,15)),("Blue Lou",(69,42)),("Blues For Alice",(41,6)),("Blues For Wood",(25,8)),("Bluesette",(49,9)),("Boo Boo's Birthday",(53,27)),("Booker's Waltz",(27,11)),("Bossa Antigua",(65,30)),("Boy Next Door",(67,26)),("Bright Mississippi",(65,21)),("Broadway",(69,11)),("Brotherhood Of Man",(37,13)),("Candy",(71,23)),("Central Park West",(43,8)),("Chucho",(57,10)),("Contemplation",(33,7)),("Cool One, The",(67,22)),("Corcovado",(75,27)),("Dancing In The Dark",(67,42)),("Dearly Beloved",(65,14)),("Don't Worry 'Bout Me",(69,29)),("Donna Lee",(67,17)),("Equinox",(23,1)),("Exactly Like You",(69,25)),("Fine And Dandy",(67,13)),("Fine Romance, A",(67,22)),("Fools Rush In",(61,20)),("Footprints",(47,8)),("Four On Six",(43,13)),("Freight Train",(41,6)),("Friday The 13th",(33,13)),("Funkallero",(37,13)),("Gary's Notebook",(61,19)),("Girl Talk",(55,30)),("Glad To Be Unhappy",(63,32)),("Good Life, The",(65,8)),("Got A Match?",(37,16)),("Hackensack",(69,23)),("Half Nelson",(33,6)),("Hot House",(63,1)),("How High The Moon",(73,3)),("Hungaria",(63,22)),("I Can't Believe...You're In Love...",(67,17)),("I Fall In Love Too Easily",(61,27)),("I Love Paris",(65,22)),("I Love You",(69,22)),("I Wish I Knew How It Would Feel To Be Free",(61,38)),("I'll Close My Eyes",(71,9)),("I've Found A New Baby",(73,15)),("I've Found A New Baby v2",(61,17)),("I've Got A Crush On You",(61,16)),("Idle Moments",(59,20)),("If You Ever Should Leave",(51,16)),("If You Never Come To Me",(29,13)),("In A Little Spanish Town",(65,25)),("In A Mellow Tone (In A Mellotone)",(61,22)),("In A Shanty In Old Shanty Town",(65,22)),("Interplay",(23,8)),("Israel",(21,11)),("It's Been A Long Long Time",(53,21)),("Jackie-ing",(35,10)),("Jeannie's Song",(65,30)),("Jersey Bounce",(71,19)),("Jody Grind, The",(23,7)),("Just A Gigolo",(41,15)),("Just Friends",(67,24)),("Just In Time",(67,31)),("Lady Bird",(29,5)),("Leaving",(37,24)),("Lennie's Pennies",(63,25)),("Light Blue",(49,23)),("Little Boat (O Barquinho)",(45,10)),("Look For The Silver Lining",(67,26)),("Mac The Knife",(33,12)),("Minor Mood",(65,40)),("Minor Strain",(27,7)),("Move",(63,31)),("Mr. P.C.",(23,1)),("My Ideal",(57,26)),("My Melancholy Baby",(63,20)),("Night Dreamer",(45,18)),("Nothing Personal",(65,32)),("Nuages",(69,30)),("O Grande Amor",(63,20)),("Old Country, The",(39,19)),("On The Trail",(31,6)),("Paper Doll",(49,18)),("Peace",(35,12)),("Pent Up House",(31,7)),("Recordame",(41,21)),("Red Clay",(25,12)),("Remember",(65,22)),("Rose Room",(63,12)),("Serenade To A Cuckoo",(51,9)),("Serenity",(43,23)),("Shadow Of Your Smile, The",(67,29)),("Shine",(63,19)),("Simone",(45,25)),("Smile",(67,27)),("So Danco Samba",(61,7)),("So Nice (Summer Samba)",(65,25)),("Solar",(29,7)),("Someday My Prince Will Come",(65,25)),("Song For My Father",(49,19)),("St. Thomas",(47,16)),("Stella By Starlight",(69,22)),("Struttin' With Some Barbecue",(63,28)),("Subconscious Lee",(63,1)),("Sugar",(31,15)),("Summertime",(41,12)),("Sunny",(33,16)),("Sweet Georgia Bright",(31,16)),("Take The A Train",(67,22)),("Them There Eyes",(67,31)),("This Can't Be Love",(71,18)),("Time On My Hands",(63,29)),("Tune Up",(63,26)),("United",(35,11)),("Valse Hot",(29,14)),("Wayne's Thang",(31,3)),("We Will Meet Again",(33,7)),("Weaver Of Dreams, A",(63,21)),("What Is This Thing Called Love",(63,5)),("When The Saints Go Marching In",(31,10)),("When You're Smilin'",(63,16)),("Why Do I Love You?",(65,20)),("Why Don't You Do Right?",(39,20)),("Witch Hunt",(51,14)),("Work Song",(33,12)),("Yesterdays",(39,12)),("You Stepped Out Of A Dream",(65,19)),("Zingaro (Retrato Em Branco E Preto)",(67,29))]


data SizeChange = SizeChange {
    pieceName :: String, 
    originalSize :: Int, 
    compressedSize :: Int} 
    deriving (Generic,Show)

instance ToJSON SizeChange 
instance FromJSON SizeChange

data SizeCurve = SizeCurve {
    step :: Int,
    corpusSize :: Int}
    deriving (Generic,Show)

instance ToJSON SizeCurve 
instance FromJSON SizeCurve

ismirExperiment = do
    ps <- pieces
    let original = corpusSLFP $ ps
        steps = compressGSteps original
        final = last steps
        ms = minedMetas final
        ruleStats = ruleSummary final
        pieceSizeComparison =  (\(k, (ori,fin)) -> SizeChange k ori fin) <$> Map.toList (individualPieceChange original final)


    encodeFile (withPath "globalMetas.json") ms
    encodeFile (withPath "ruleStats.json")  ruleStats
    encodeFile (withPath "pieceSizeComparison.json") pieceSizeComparison
    encodeFile (withPath "sizeCurve.json") (uncurry SizeCurve <$> zip [1..] (size <$>steps))
    pure ms
    where withPath x = "experiment/result/" ++ x
    

-- >>> ismirExperiment
-- [("RG1",[_,0]),("RG4",[★,_]),("RG9",[_,_,0,1]),("RG13",[★]),("RG16",[_,★]),("RG17",[_,0,0,0]),("RG20",[_,_,0,_]),("RG27",[_,_,_,1]),("RG61",[_,0,_,2,2,2]),("RG62",[_,0,_,0]),("RG63",[_,0,_,_]),("RG64",[_,0,_]),("RG65",[_,_,_,0]),("RG100",[_,_,0,0]),("RG101",[_,_,_,2]),("RG148",[_,_,1,_]),("RG215",[_,_,0,1,_,_]),("RG216",[_,_,_,0,_,0]),("RG217",[_,_,_,_,0,1,_]),("RG218",[_,_,0,_,_,_])]


-- M_{1} &= \langle \_,1 \rangle &

-- plotCompressionExample = do 
--     ps <-  pieces 
--     let original = corpusSLFP $ take 5 ps
--         final = fixedPoint compressG original
--         decompressed = fixedPoint deCompressG final
--     writeSVG "JazzCorpusCompression(compressed).svg" (plotSLFP final)
    
--     writeSVG "JazzCorpusCompression(decompressed).svg" (plotSeqDiagrams $ plotSLFP <$> iterateFs [deCompressG |i<- [1 ..] ] final)
    
-- -- >>>  plotCompressionExample
