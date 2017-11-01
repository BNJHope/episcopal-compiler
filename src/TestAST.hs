module TestAST
(
getTestASTs
) where

import Structures

getTestASTs :: [Program]
getTestASTs = [getTestAST1, getTestAST2, getTestAST3, getTestAST4]

getTestAST1 :: Program
getTestAST1 = Program "TestConst" constant1 []

getTestAST2 :: Program
getTestAST2 = Program "TestFunc" func1 []

getTestAST3 :: Program
getTestAST3 = Program "TestNestedFunc" func2 []

getTestAST4 :: Program
getTestAST4 = Program "TestQuery" (funcCallSum constant42 constant42) [querySum]

func1 :: Expr
func1 = ExprDef [funcDefSum] (funcCallSum constant42 constant42)

func2 :: Expr
func2 = ExprDef [funcDefSum, funcDefIncrement] $ funcCallIncrement constant42

querySum :: Query
querySum = Query "sum" ["x", "y"] [addXY]

funcDefIncrement :: Definition
funcDefIncrement = (FuncDef "increment" ["z"] [(funcCallSum (ExprReference "z") constant1)])

funcDefSum :: Definition
funcDefSum = (FuncDef "sum" ["x", "y"] [addXY])

addXY :: Expr
addXY = ExprBinOp $ BinOp ADD (ExprReference "x") (ExprReference "y")

funcCallSum :: Expr -> Expr -> Expr
funcCallSum expr1 expr2 = ExprFunctionCall "sum" [expr1, expr2]

funcCallIncrement :: Expr -> Expr
funcCallIncrement expr1 = ExprFunctionCall "increment" [expr1]

constant1 :: Expr
constant1 = ExprConstant $ EInt 1

constant42 :: Expr
constant42 = ExprConstant $ EInt 42
