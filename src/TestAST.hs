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
    ast13,
    ast14,
    ast15,
    ast16,
    ast17,
    ast18,
    ast19,
    ast20,
    ast21,
    ast22,
    ast23]

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
ast10 = Program "TestFlip" exprFlipHalf []

ast11 :: Program
ast11 = Program "TestSampleFlip" (ExprSample exprFlipHalf) []

ast12 :: Program
ast12 = Program "TestObservationFalse" (ExprObserve constantFalse constant42) []

ast13 :: Program
ast13 = Program "TestObservationTrue" (ExprObserve constantTrue constant42) []

ast14 :: Program
ast14 = Program "TestSubtract" subtractFortyTwoFive []

ast15 :: Program
ast15 = Program "TestMultiply" multiplyFortyTwoFive []

ast16 :: Program
ast16 = Program "TestOrTrueFalse" orTrueFalse []

ast17 :: Program
ast17 = Program "TestOrTrueTrue" orTrueTrue []

ast18 :: Program
ast18 = Program "TestOrFalseFalse" orFalseFalse []

ast19 :: Program
ast19 = Program "TestAndTrueFalse" andTrueFalse []

ast20 :: Program
ast20 = Program "TestAndTrueTrue" andTrueTrue []

ast21 :: Program
ast21 = Program "TestAndFalseFalse" andFalseFalse []

ast22 :: Program
ast22 = Program "Test42GreaterThan5" fortyTwoGreaterThanFive []

ast23 :: Program
ast23 = Program "Test5GreaterThan42" fiveGreaterThanFortyTwo []

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

subtractFortyTwoFive :: Expr
subtractFortyTwoFive = ExprBinOp $ BinOp SUBTRACT (constant42) (constantFive)

multiplyFortyTwoFive :: Expr
multiplyFortyTwoFive = ExprBinOp $ BinOp MULT (constant42) (constantFive)

overFortyTwoAndTwo :: Expr
overFortyTwoAndTwo = ExprBinOp $ BinOp MULT (constant42) (constantTwo)

orTrueFalse :: Expr
orTrueFalse = ExprBinOp $ BinOp OR (constantTrue) (constantFalse)

orTrueTrue :: Expr
orTrueTrue = ExprBinOp $ BinOp OR (constantTrue) (constantTrue)

orFalseFalse :: Expr
orFalseFalse = ExprBinOp $ BinOp OR (constantFalse) (constantFalse)

andTrueFalse :: Expr
andTrueFalse = ExprBinOp $ BinOp AND (constantTrue) (constantFalse)

andTrueTrue :: Expr
andTrueTrue = ExprBinOp $ BinOp AND (constantTrue) (constantTrue)

andFalseFalse :: Expr
andFalseFalse = ExprBinOp $ BinOp AND (constantFalse) (constantFalse)

fortyTwoGreaterThanFive :: Expr
fortyTwoGreaterThanFive = ExprBinOp $ BinOp GREATER_THAN (constant42) (constantFive)

fiveGreaterThanFortyTwo :: Expr
fiveGreaterThanFortyTwo = ExprBinOp $ BinOp GREATER_THAN (constantFive) (constant42)

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

constantTwo :: Expr
constantTwo = ExprConstant $ EInt 2

constantTrue :: Expr
constantTrue = ExprConstant $ EBoolean True

constantFalse :: Expr
constantFalse = ExprConstant $ EBoolean False

constant31 :: Expr
constant31 = ExprConstant $ EInt 31
