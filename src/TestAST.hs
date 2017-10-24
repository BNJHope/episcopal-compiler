module TestAST
(
getTestAST
) where

import Structures

getTestAST :: AST
getTestAST = constant1

constant1 :: Constant
constant1 = EInt 1
