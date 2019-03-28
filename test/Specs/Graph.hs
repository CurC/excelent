module Spec.Graph where

import Test.Tasty
import Test.Tasty.QuickCheck
import Test.QuickCheck

import qualified Data.Map as M
import qualified Algebra.Graph.AdjacencyMap as GA
import qualified Algebra.Graph.AdjacencyMap.Algorithms as GA

import Excelent.Definition
import Excelent.Eval.Graph
import Spec.Arbitrary

graphProps :: TestTree
graphProps = testGroup "Graph" [constProps, refProps, addProps]

nodeCountProps :: TestTree
nodeCountProps = testGroup "# of nodes"
  [
      testProperty "# nodes from literals" literalNodes,
      testProperty "# nodes from references" refNodes,
      testProperty "# nodes from operations" opEdges,
  ]

edgeCountProps :: TestTree
edgeCountProps = testGroup "# of edges"
  [
      testProperty "# edges from literals" literalEdges,
      testProperty "# edges from references" refEdges,
      testProperty "# edges from operations" opEdges,
  ]

literalNodes :: Property
literalNodes = forAll genInt $ \i -> 1 == vertexCount i

literalEdges :: Property
literalEdges = forAll genInt $ \i -> 0 == edgeCount i

refNodes :: Property
refNodes = forAll genRef $ \r -> 2 == vertexCount r

refEdges :: Property
refEdges = forAll genRef $ \r -> 1 == edgeCount r

opNodes :: (Int, Int) -> Int -> Property
opNodes = forAll genOp $ \op -> 3 == vertexCount op

opEdges :: Int -> Property
opEdges = forAll genOp $ \op -> 2 == edgeCount op
