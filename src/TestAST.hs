module TestAST
(
getTestAST
) where

import Structures

getTestAST :: AST
getTestAST = constant1

constant1 :: Expression
constant1 = EConstant $ EInt 42
