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
minedMetas = sortOn ( metaRuleNameToInt .fst) . Map.toList . globalMetas

-- >>> (minedMetas . fixedPoint compressG . corpusSLFP) <$> pieces
-- [("RG1",[_,1]),("RG7",[★,_]),("RG15",[★]),("RG17",[_,_,2]),("RG31",[_,★]),("RG45",[_,1,_]),("RG95",[★,★]),("RG115",[_,1,1]),("RG205",[_,_,1]),("RG312",[_,_,1,_]),("RG313",[_,_,_,1]),("RG314",[_,_,_,_,3])]

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
-- (68,0,302,12)



individualPieceChange :: Ord k => SLFP a1 k -> SLFP a2 k -> Map.Map k (Int, Int)
individualPieceChange slfpOrig selfFin = Map.intersectionWith (,) (size <$> sltps slfpOrig) (size <$> sltps selfFin)

-- >>> individualPieceChange . corpusSLFP <$> pieces
-- fromList [("502 Blues",(69,21)),("Across The Valley From The Alamo",(73,20)),("Afro Blue",(39,11)),("All Of Me",(61,9)),("Armando's Rhumba",(43,12)),("Autumn Leaves",(67,12)),("Avalon",(63,11)),("Backstage Sally",(57,11)),("Ballade",(49,12)),("Baubles, Bangles And Beads",(71,17)),("Beatrice",(33,11)),("Beautiful Friendship, A",(67,13)),("Beautiful Love",(71,17)),("Bemsha Swing",(57,15)),("Best Things In Life Are Free, The",(73,12)),("Bill Bailey",(65,11)),("Birk's Works",(31,5)),("Blue Bossa",(29,4)),("Blue In Green",(31,8)),("Blue Lou",(69,23)),("Blues For Alice",(41,6)),("Blues For Wood",(25,5)),("Bluesette",(49,9)),("Boo Boo's Birthday",(53,22)),("Booker's Waltz",(27,8)),("Bossa Antigua",(65,17)),("Boy Next Door",(67,16)),("Bright Mississippi",(65,16)),("Broadway",(69,11)),("Brotherhood Of Man",(37,8)),("Candy",(71,19)),("Central Park West",(43,12)),("Chucho",(57,7)),("Contemplation",(33,6)),("Cool One, The",(67,16)),("Corcovado",(75,15)),("Dancing In The Dark",(67,24)),("Dearly Beloved",(65,10)),("Don't Worry 'Bout Me",(69,14)),("Donna Lee",(67,15)),("Equinox",(23,1)),("Exactly Like You",(69,16)),("Fine And Dandy",(67,13)),("Fine Romance, A",(67,17)),("Fools Rush In",(61,12)),("Footprints",(47,8)),("Four On Six",(43,9)),("Freight Train",(41,6)),("Friday The 13th",(33,7)),("Funkallero",(37,9)),("Gary's Notebook",(61,15)),("Girl Talk",(55,18)),("Glad To Be Unhappy",(63,19)),("Good Life, The",(65,13)),("Got A Match?",(37,8)),("Hackensack",(69,11)),("Half Nelson",(33,2)),("Hot House",(63,1)),("How High The Moon",(73,17)),("Hungaria",(63,14)),("I Can't Believe...You're In Love...",(67,12)),("I Fall In Love Too Easily",(61,14)),("I Love Paris",(65,11)),("I Love You",(69,12)),("I Wish I Knew How It Would Feel To Be Free",(61,26)),("I'll Close My Eyes",(71,10)),("I've Found A New Baby",(73,8)),("I've Found A New Baby v2",(61,9)),("I've Got A Crush On You",(61,13)),("Idle Moments",(59,11)),("If You Ever Should Leave",(51,9)),("If You Never Come To Me",(29,9)),("In A Little Spanish Town",(65,14)),("In A Mellow Tone (In A Mellotone)",(61,13)),("In A Shanty In Old Shanty Town",(65,14)),("Interplay",(23,4)),("Israel",(21,3)),("It's Been A Long Long Time",(53,14)),("Jackie-ing",(35,6)),("Jeannie's Song",(65,16)),("Jersey Bounce",(71,14)),("Jody Grind, The",(23,4)),("Just A Gigolo",(41,10)),("Just Friends",(67,11)),("Just In Time",(67,13)),("Lady Bird",(29,2)),("Leaving",(37,12)),("Lennie's Pennies",(63,17)),("Light Blue",(49,14)),("Little Boat (O Barquinho)",(45,7)),("Look For The Silver Lining",(67,18)),("Mac The Knife",(33,7)),("Minor Mood",(65,23)),("Minor Strain",(27,6)),("Move",(63,14)),("Mr. P.C.",(23,1)),("My Ideal",(57,19)),("My Melancholy Baby",(63,17)),("Night Dreamer",(45,14)),("Nothing Personal",(65,12)),("Nuages",(69,14)),("O Grande Amor",(63,13)),("Old Country, The",(39,12)),("On The Trail",(31,4)),("Paper Doll",(49,13)),("Peace",(35,8)),("Pent Up House",(31,4)),("Recordame",(41,7)),("Red Clay",(25,7)),("Remember",(65,16)),("Rose Room",(63,12)),("Serenade To A Cuckoo",(51,8)),("Serenity",(43,13)),("Shadow Of Your Smile, The",(67,18)),("Shine",(63,13)),("Simone",(45,16)),("Smile",(67,18)),("So Danco Samba",(61,13)),("So Nice (Summer Samba)",(65,18)),("Solar",(29,3)),("Someday My Prince Will Come",(65,23)),("Song For My Father",(49,11)),("St. Thomas",(47,7)),("Stella By Starlight",(69,15)),("Struttin' With Some Barbecue",(63,11)),("Subconscious Lee",(63,1)),("Sugar",(31,6)),("Summertime",(41,11)),("Sunny",(33,8)),("Sweet Georgia Bright",(31,8)),("Take The A Train",(67,8)),("Them There Eyes",(67,14)),("This Can't Be Love",(71,14)),("Time On My Hands",(63,13)),("Tune Up",(63,17)),("United",(35,7)),("Valse Hot",(29,5)),("Wayne's Thang",(31,8)),("We Will Meet Again",(33,6)),("Weaver Of Dreams, A",(63,13)),("What Is This Thing Called Love",(63,3)),("When The Saints Go Marching In",(31,8)),("When You're Smilin'",(63,9)),("Why Do I Love You?",(65,14)),("Why Don't You Do Right?",(39,8)),("Witch Hunt",(51,11)),("Work Song",(33,7)),("Yesterdays",(39,8)),("You Stepped Out Of A Dream",(65,11)),("Zingaro (Retrato Em Branco E Preto)",(67,22))]


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

-- plotCompressionExample = do 
--     ps <-  pieces 
--     let original = corpusSLFP $ take 5 ps
--         final = fixedPoint compressG original
--         decompressed = fixedPoint deCompressG final
--     writeSVG "JazzCorpusCompression(compressed).svg" (plotSLFP final)
    
--     writeSVG "JazzCorpusCompression(decompressed).svg" (plotSeqDiagrams $ plotSLFP <$> iterateFs [deCompressG |i<- [1 ..] ] final)
    
-- -- >>>  plotCompressionExample
