module Spec.Arbitrary where

import Test.Tasty
import Test.Tasty.QuickCheck
import Test.QuickCheck
import Test.QuickCheck.Gen

import Excelent.Definition

instance Arbitrary Expr where
    arbitrary = do
        oneof [genInt, genRef, genOp]

genInt :: Gen Expr
genInt = do
    i <- arbitrary
    return $ ConstInt i

genRef :: Gen Expr
genRef = do
    pos <- arbitrary
    elements [RefRel pos, RefAbs pos]

genOp :: Gen Expr
genOp = do
    (e1, e2) <- arbitrary
    return $ Plus e1 e2
