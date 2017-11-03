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
    ast12,
    ast13,
    ast15,
    ast15,
    ast16,
    ast17,
    ast18,
    ast19,
    ast20,
    ast21,
    ast22,
    ast23,
    ast24,
    ast25,
    ast26,
    ast27,
    ast28,
    ast29,
    ast30]

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

ast12 :: Program
ast12 = Program "TestFlip" exprFlipHalf []

ast13 :: Program
ast13 = Program "TestSampleFlip" (ExprSample exprFlipHalf) []

ast15 :: Program
ast15 = Program "TestObservationFalse" (ExprObserve constantFalse constant42) []

ast16 :: Program
ast16 = Program "TestObservationTrue" (ExprObserve constantTrue constant42) []

ast17 :: Program
ast17 = Program "TestSubtract" subtractFortyTwoFive []

ast18 :: Program
ast18 = Program "TestMultiply" multiplyFortyTwoFive []

ast19 :: Program
ast19 = Program "TestOrTrueFalse" orTrueFalse []

ast20 :: Program
ast20 = Program "TestOrTrueTrue" orTrueTrue []

ast21 :: Program
ast21 = Program "TestOrFalseFalse" orFalseFalse []

ast22 :: Program
ast22 = Program "TestAndTrueFalse" andTrueFalse []

ast23 :: Program
ast23 = Program "TestAndTrueTrue" andTrueTrue []

ast24 :: Program
ast24 = Program "TestAndFalseFalse" andFalseFalse []

--
ast25 :: Program
ast25 = Program "Test42GreaterThan5" fortyTwoGreaterThanFive []

ast26 :: Program
ast26 = Program "Test42LessThan5" fortyTwoLessThanFive []

ast27 :: Program
ast27 = Program "Test5GreaterThan42" fiveGreaterThanFortyTwo []

ast28 :: Program
ast28 = Program "Test5LessThan42" fiveLessThanFortyTwo []

ast29 :: Program
ast29 = Program "Test5Equals42" fiveEqualsFortyTwo []

ast30 :: Program
ast30 = Program "Test5Equals5" fiveEqualsFive []
--

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

fortyTwoLessThanFive :: Expr
fortyTwoLessThanFive = ExprBinOp $ BinOp LESS_THAN (constant42) (constantFive)

fiveGreaterThanFortyTwo :: Expr
fiveGreaterThanFortyTwo = ExprBinOp $ BinOp GREATER_THAN (constantFive) (constant42)

fiveLessThanFortyTwo :: Expr
fiveLessThanFortyTwo = ExprBinOp $ BinOp LESS_THAN (constantFive) (constant42)

fiveEqualsFortyTwo :: Expr
fiveEqualsFortyTwo = ExprBinOp $ BinOp EQUALS (constantFive) (constant42)

fiveEqualsFive :: Expr
fiveEqualsFive = ExprBinOp $ BinOp EQUALS (constantFive) (constantFive)

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
