module TestAST
(
getTestAST
) where

import Structures

getTestAST :: Expression
getTestAST = constant1

constant1 :: Expression
constant1 = EExpr $ ExprConstant $ EInt 42
