module Specs.Graph where

import Test.Tasty
import Test.Tasty.QuickCheck
import Test.QuickCheck

import qualified Data.Map as M
import qualified Algebra.Graph.AdjacencyMap as GA

import Excelent.Definition
import Excelent.Eval.Graph
import Specs.Arbitrary

graphProps :: TestTree
graphProps = testGroup "Graph" [nodeCountProps, edgeCountProps]

nodeCountProps :: TestTree
nodeCountProps = testGroup "# of nodes" [
        testProperty "# nodes from literals" literalNodes,
        testProperty "# nodes from references" refNodes,
        testProperty "# nodes from operations" opEdges
    ]

edgeCountProps :: TestTree
edgeCountProps = testGroup "# of edges" [
        testProperty "# edges from literals" literalEdges,
        testProperty "# edges from references" refEdges,
        testProperty "# edges from operations" opEdges
    ]

literalNodes :: Property
literalNodes = forAll genInt $ \i -> 1 == GA.vertexCount (node i (0, 0))
literalEdges :: Property
literalEdges = forAll genInt $ \i -> 0 == GA.edgeCount (node i (0, 0))

refNodes :: Property
refNodes = forAll genRef $ \r -> 2 >= GA.vertexCount (node r (0, 0))
refEdges :: Property
refEdges = forAll genRef $ \r -> 1 >= GA.edgeCount (node r (0, 0))

opNodes :: Property
opNodes = forAll genOpNode $ \op -> 3 >= GA.vertexCount (node op (0, 0))

opEdges :: Property
opEdges = forAll genOpNode $ \op -> 2 >= GA.edgeCount (node op (0, 0))
