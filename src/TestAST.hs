module TestAST
(
getTestASTs
) where

import Structures

getTestASTs :: [Program]
getTestASTs = [ast1,
    ast2,
    ast3,
    ast4,
    ast5,
    ast6,
    ast7,
    ast8,
    ast9,
    ast10,
    ast11,
    ast12,
    ast13]

ast1 :: Program
ast1 = Program "TestConst" constant1 []

ast2 :: Program
ast2 = Program "TestFunc" func1 []

ast3 :: Program
ast3 = Program "TestNestedFunc" func2 []

ast4 :: Program
ast4 = Program "TestQuery" (funcCallSum constant42 constant42) [querySum]

ast5 :: Program
ast5 = Program "TestNestedFuncInQuery" (funcCallIncrement constant42) [queryIncrement]

ast6 :: Program
ast6 = Program "TestBernoulli" exprBernoulliHalf []

ast7 :: Program
ast7 = Program "TestSampleBernoulli" (ExprSample exprBernoulliHalf) []

ast8 :: Program
ast8 = Program "TestBeta" exprBetaFiveTwelve []

ast9 :: Program
ast9 = Program "TestSampleBeta" (ExprSample exprBetaFiveTwelve) []

ast10 :: Program
ast10 = Program "TestNormal" exprNormalFiveHalve []

ast11 :: Program
ast11 = Program "TestSampleNormal" (ExprSample exprNormalFiveHalve) []

ast12 :: Program
ast12 = Program "TestFlip" exprFlipHalf []

ast13 :: Program
ast13 = Program "TestSampleFlip" (ExprSample exprFlipHalf) []

func1 :: Expr
func1 = ExprDef [funcDefSum] (funcCallSum constant42 constant42)

func2 :: Expr
func2 = ExprDef [funcDefSum, funcDefIncrement] $ funcCallIncrement constant42

querySum :: Query
querySum = Query "sum" ["x", "y"] [addXY]

queryIncrement :: Query
queryIncrement = Query "increment" ["z"] [ExprDef [funcDefSum] (funcCallSum (ExprReference "z") constant1)] 

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

exprBernoulliHalf :: Expr
exprBernoulliHalf = (ExprDistrib $ bernoulli constantHalf)

exprBetaFiveTwelve :: Expr
exprBetaFiveTwelve = (ExprDistrib $ beta constantFive constantTwelve)

exprNormalFiveHalve :: Expr
exprNormalFiveHalve = (ExprDistrib $ normal constantFive constantHalf)

exprFlipHalf :: Expr
exprFlipHalf = (ExprDistrib $ flipDist constantHalf)

bernoulli :: Expr -> Distribution
bernoulli p = DistBernoulli p

beta :: Expr -> Expr -> Distribution
beta alpha beta = DistBeta alpha beta

normal :: Expr -> Expr -> Distribution
normal mean stanDev = DistNormal mean stanDev

flipDist :: Expr -> Distribution
flipDist p = DistFlip p

constant1 :: Expr
constant1 = ExprConstant $ EInt 1

constant42 :: Expr
constant42 = ExprConstant $ EInt 42

constantHalf :: Expr
constantHalf = ExprConstant $ EFloat 0.5

constantFive :: Expr
constantFive = ExprConstant $ EInt 5

constantTwelve :: Expr
constantTwelve = ExprConstant $ EInt 12
