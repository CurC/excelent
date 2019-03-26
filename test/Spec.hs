import Test.Tasty

import Specs.Eval

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [evalProps]
