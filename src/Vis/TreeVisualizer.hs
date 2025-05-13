{-# LANGUAGE ScopedTypeVariables #-}

module Vis.TreeVisualizer () where
import Diagrams
import Diagrams.Backend.SVG
import Diagrams.Prelude
import Diagrams.TwoD.Layout.Grid
import Diagrams.TwoD.Layout.Tree
import Data.Tree (Tree(..))
import Data.Map (Map)
import Data.List
import qualified Data.Map as Map
import Data.Graph (graphFromEdges', topSort)
import Diagrams.Color.XKCD (blueGreen)
import Data.List.Split (sepBy)
import Data.Maybe (fromMaybe)





closer :: (Functor f, Num a, Num (f a)) => a -> (f a -> f a -> t) -> f a -> f a -> t
closer r f x y = f (x + (r *^ d)) (y - (r *^ d))
  where
    d = y - x

treeDiagram lay x =
  renderTree
    (\t -> text t # fc black # fontSizeL 0.3) -- <> rect 1 0.5 # lwL 0.025  )   
    (closer 0.25 (~~)  # lwL 0.025  ) -- # lw 0.25 
    (lay x)
    # centerXY
    # frame 1
    # pad 1.2
    # bg white
    
treeDiagram' :: _ => (a -> Diagram B) -> Tree a -> Diagram B
treeDiagram' (f::a -> Diagram B) (x::Tree a) =
  renderTree
    f 
    (closer 0.25 (~~)  # lwL 0.05  ) -- # lw 0.25 
    (symmLayout' (with & slHSep .~ 3
                       & slVSep .~ 3
                       & slWidth  .~ fromMaybe (0,0) . extentX
                       & slHeight .~ fromMaybe (0,0) . extentY) 
                  x)
    # centerXY
    -- # frame 1
    -- # pad 1.2
    # bg white

type ProofTree = Tree (String,String) -- (Type String, Constructor String)

vsepRule d x y = vsep 1 [x, d <> hrule n ,y] where 
  n = max (width x) (width y)

vsepTopRule x d y = vsep 1 [x <> hrule n, d , y] <> rect n 1 where 
  n = max (width x) (width y)


proofTreeDiagram :: ProofTree -> Diagram B
proofTreeDiagram x = proofTreeDiagram' x # centerXY # pad 1.1 # bg white

proofTreeDiagram' :: ProofTree -> Diagram B
proofTreeDiagram' (Node (x,c) ts) =  vsepRule (text c # fontSizeL 0.5 # fc gray<> rect 4 1 # bg white) (text x # fontSizeL 1) ( (centerX . hsep 1) (proofTreeDiagram' <$> ts))
  # lw 0.5
  # lc gray




upperEnclose d x y = appends (x <> (rect n 2 # lw none)) $ zip (iterateN 4 (rotateBy (1/4)) unitX) [
    vrule 3,
    d <> hrule n ,
    vrule 3,
    y
  ]
  where n = max (width d+1) (width y)


addTopHLine d x = vsep 1 [hrule (width x) <> d, x]

showBoundingBox :: Diagram B -> Diagram B
showBoundingBox x = x <> boundingRect x

-- toTreeDiagram :: ToTree a => a -> Diagram B
-- toTreeDiagram = toTreeDiagram' . toTree'

toTreeDiagram' :: Tree String -> Diagram B
toTreeDiagram'  = treeDiagram symmLayout



patternLayout :: [Diagram B] -> Diagram B
patternLayout ds = position $ zip  (iterate (rotateBy (1/n)) (scaling *^ unitX)) (sizedAs (circle 1 :: D V2 Double) <$> ds)  where 
  n = fromIntegral $ length ds
  scaling = 2*n*pi

dependencyDiagram :: (Ord a, Show a) => ([Diagram B] -> Diagram B) -> (a->Diagram B) -> [(a,[a])] -> Diagram B
dependencyDiagram lay f ps = lay (sameBoundingRect  ( (\x -> f x # pad 1.1 # fontSizeL 0.3 # centerXY # frame 1 # (\x -> x <>boundingRect x ) # lwL 0.025   # named (show x)  ) <$> sorted))
  # applyAll (mconcat [(\n -> connectOutside' opts (nameS p) n ) <$> nameTs p | p <-  ps])
  # pad 1.1
  # bg white
  
  where
    sorted = fst <$> topSort' ps 
    nameS  (a,_) = show a
    nameTs (_,as) = show <$> as
    opts = with 
      -- & arrowShaft .~ arc xDir (-1/8 @@ turn)
      & headTexture .~ solid gray 
      & headLength .~  local 0.3
      & shaftStyle %~ lwL 0.05
      -- & shaftStyle %~ lc gray 
      & shaftTexture .~ gradient

stops = mkStops [(teal, 0, 1), (orange, 1, 1)]
gradient = defaultLG 
  & _LG . lGradStops .~ stops



dependencyDiagram' :: _ => (a->Diagram B) -> Map a [a] -> Diagram B
dependencyDiagram' f m = dependencyDiagram patternLayout f $ topSort' (Map.toList  m)

topSort' :: Ord a => [(a, [a])] -> [(a,[a])]
topSort' l = g'. nodeFromVertex <$> topSort graph
  where 
    g (a,b) = (a,a,b)
    g' (_,a,b) = (a,b)
    (graph,nodeFromVertex) = graphFromEdges' . fmap g $ l


manyDiagram :: [Diagram B] -> Diagram B
manyDiagram l =  (frame 1 <$> l)
  # sameBoundingRect
  # gridCat' 3
  # bg white
  # centerXY

niceVsep :: [Diagram B] -> Diagram B
niceVsep l = if null l then rect 1 1 # lwL 0.0  else l
  # fmap (frame 1)
  # sameBoundingRect
  # gridCat' 1
  # bg white
  # centerXY

niceHsep :: [Diagram B] -> Diagram B
niceHsep l = if null l then rect 1 1 # lwL 0.0 else l
  # fmap (frame 1)
  -- # sameBoundingRect
  # gridCat' (length l)
  # bg white
  # centerXY


manyDiagram' ::[Diagram B] -> Diagram B
manyDiagram' l = if null l then rect 1 1 else (frame 1 <$> l)
  # sameBoundingRect
  # gridCat' 10
  # bg white
  # centerXY


writeSVG p = renderSVG p (dims2D (1410*r) (1000*r))
  where r = 1

plotSeqDiagrams :: [Diagram B] -> Diagram B
plotSeqDiagrams ds = ds
    # rememberOrder
    # map (frame 1)
    # gridSnake' 1
    # showOrder
    # bg white
    # centerXY

rememberOrder :: [Diagram B] -> [Diagram B]
rememberOrder = zipWith named [0::Int ..]

showOrder :: Diagram B -> Diagram B
showOrder diagram
  = diagram # applyAll (map addArrow [0 .. length (names diagram)])
  where
    addArrow n = connectOutside' opts  n (n + 1)
    opts = with & gaps .~ none -- normalized 0.005
               & headTexture .~ solid gray
               & headLength .~  local 0.3
               & shaftStyle %~ lwL 0.05
               & shaftStyle %~ lc gray

rRect x = roundedRect (width x) (height x ) 0.0 # lwL 0.01 
encloseWith f t = t # centerXY # frame 0.1 # (\x -> x <> f x)
enclose t = encloseWith rRect t

