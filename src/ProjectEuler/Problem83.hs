{-# LANGUAGE
    ScopedTypeVariables
  , FlexibleContexts
  #-}
module ProjectEuler.Problem83
  ( problem
  ) where

import Control.Monad
import Control.Monad.ST
import Control.Monad.State
import Petbox

import qualified Data.Array.ST as A
import qualified Data.Array.Unboxed as A
import qualified Data.PSQueue as PS
import qualified Data.Set as S
import qualified Data.Text as T

import ProjectEuler.Types

problem :: Problem
problem = pureProblemWithData "p083_matrix.txt" 83 Solved compute

{-
  let's convert the problem into a one source shortest path problem:
  - vertices are cells represented by coordinates.
    there is one extra coordinate: (0,0), which points to (1,1)
    vertex connecting with it. we want to find the shorted path
    from (0,0) to (rowN,colN)
  - edges are directed, from (x,y) it can go to its 4 neighborhoods,
    the edge weight is the weight of the cell that arc points to.
  - whenever you take an edge, it means we are making the decision
    of going to the cell (x,y) and taking the weight on that cell
  - there shouldn't be a loop in the solution,
    because if there is one, we can shortcut it and get less weight.
    so the shortest path algorithm should work
-}

type Vertex = (Int,Int)
type Graph = A.UArray (Int,Int) Int

getMat :: T.Text -> Graph
getMat raw = A.array ((1,1), (rowN,colN)) genPairs
  where
    raws = map parseLine . lines . T.unpack $ raw
    parseLine s = read ("[" ++ s ++ "]") :: [Int]
    colN = length (head raws)
    rowN = length raws
    genPairs = concat $ add2DCoords 1 1 raws


getEdges :: Graph -> Vertex -> [(Vertex, Int)]
getEdges ary v1 = case v1 of
    (0,0) -> [((1,1), ary A.! (1,1))]
    (x,y) -> map (keepInput (ary A.!))
                 (concat [[(x-1,y  ) | x-1 >= 1   ]
                         ,[(x+1,y  ) | x+1 <= rowN]
                         ,[(x  ,y-1) | y-1 >= 1   ]
                         ,[(x  ,y+1) | y+1 <= colN]
                         ])
  where
    (_,(rowN,colN)) = A.bounds ary

findShortestPath :: Graph -> A.UArray Int Int
findShortestPath ary = A.runSTUArray build
 where
   -- examining the data and we can see that
   -- the maximum cell value is 9999
   -- so we know how large the "fake infinity" should be
   -- p.s.: maxBound might overflow when doing addition
   inf = rowN * colN * 10000 * 10
   allVertices = (0,0):[(x,y) | x<-[1..rowN], y<-[1..colN]]
   vSize = A.rangeSize bd
   bd@(_,(rowN,colN)) = A.bounds ary
   build :: forall s. ST s (A.STUArray s Int Int)
   build = do
       -- dist[_] = inf
       mdist <- A.newArray (0,vSize) inf
       let idxConvert (0,0) = 0
           idxConvert (i,j) = 1 + A.index bd (i,j)
           writeArr i = A.writeArray mdist (idxConvert i)
           readArr  i = A.readArray  mdist (idxConvert i)
       -- dist[(0,0)] = 0
       writeArr (0,0) 0
       let -- same as dist
           initPQ = PS.adjust (const 0) (0,0) -- dist[(0,0)] = 0
                  . PS.fromList
                  . map (PS.:-> inf)
                  $ allVertices
           dijkstra :: PS.PSQ Vertex Int
                    -> S.Set Vertex
                    -> ST s ()
           dijkstra pq visited = case PS.minView pq of
               -- when nothing's in the queue
               Nothing -> pure ()
               -- otherwise delete (view) min
               Just (u PS.:-> w,pq1) -> do
                   -- mark as visited, construct search space
                   let newVisited = S.insert u visited
                       searchSpace = filter ((`S.notMember` newVisited) . fst)
                                  $ getEdges ary u
                   -- pq need the be updated frequently, therefore StateT
                   let getNewPriQueue = flip execStateT pq1 $
                         forM_ searchSpace $ \(v,w2) -> do
                             let alt = w + w2
                             distV <- lift $ readArr v
                             -- relax
                             when (alt < distV) $ do
                                 lift $ writeArr v alt
                                 modify (PS.adjust (const alt) v)
                   pq2 <- getNewPriQueue
                   dijkstra pq2 newVisited
       dijkstra initPQ S.empty
       pure mdist

compute :: T.Text -> Int
compute raw = dist A.! (1+A.index bd vm)
  where
    g = getMat raw
    dist = findShortestPath g
    bd@(_,vm) = A.bounds g

