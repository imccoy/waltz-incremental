module Graph (showGraph) where

import Data.Data (toConstr)
import Data.Maybe (isJust, fromJust)
import qualified Data.Array as Array

import qualified Data.Graph.Inductive.Graph as Gr
import qualified Data.Graph.Inductive.PatriciaTree as Gr
import qualified Data.Graph.Inductive.Graphviz as Gr

import Types

showGraph :: HeapStore -> String
showGraph g = Gr.graphviz' $ makeGraph g

makeGraph heapStore = makeEdges heapStore $ makeNodes heapStore

makeNodes :: HeapStore -> Gr.Gr String String
makeNodes heapStore = Gr.insNodes (map makeNode $ cleanup $ Array.assocs heapStore)
                                  Gr.empty
  where makeNode :: (HeapValue, (Value, Dependents)) -> (Int, String)
        makeNode (i, e) = (fromIntegral i, (graphValue . fst) e)

makeEdges :: HeapStore -> Gr.Gr String String -> Gr.Gr String String
makeEdges heapStore gr = Gr.insEdges (concatMap go $ cleanup $ Array.assocs heapStore)
                                     gr
  where go (i, e) = map (makeEdge i) $ snd e
        makeEdge i (hv, v) = (fromIntegral i, fromIntegral hv, "")

cleanup :: [(a, Maybe b)] -> [(a, b)]
cleanup = (map (\(a, b) ->  (a, fromJust b))) . (filter (\(_,b) -> isJust b))

graphValue (DataValue (_, dcon) _) = "DataValue " ++ dcon
graphValue (IntegralValue i) = "IntegralValue " ++ show i
graphValue (CharValue c) = "CharValue " ++ show c
graphValue (StringValue s) = "StringValue '" ++ s ++ "'"
graphValue (Thunk {thunkExp = (RtExp _ s _)}) = "RtExp " ++ s 
graphValue (Thunk {thunkExp = (CoreExp exp)}) = "CoreExp " ++ show (toConstr exp)
