module TestAST
(
getTestASTs
) where

import Structures

getTestASTs :: [Program]
getTestASTs = [getTestAST1, getTestAST2]

getTestAST1 :: Program
getTestAST1 = Program "TestConst" constant1 []

getTestAST2 :: Program
getTestAST2 = Program "TestFunc" func1 []

func1 :: Expr
func1 = ExprDef [funcDefSum] funcCallSum

funcDefSum :: Definition
funcDefSum = (FuncDef "sum" ["x", "y"] [addXY])

addXY :: Expr
addXY = ExprBinOp $ BinOp ADD (ExprReference "x") (ExprReference "y")

funcCallSum :: Expr
funcCallSum = ExprFunctionCall "sum" [constant1, constant1]

constant1 :: Expr
constant1 = ExprConstant $ EInt 42
