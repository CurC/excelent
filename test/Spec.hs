import Test.Tasty

import Specs.Eval
import Specs.Graph

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [evalProps, graphProps]
