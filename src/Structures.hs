module Structures(
ID,
VariableSet,
FunctionResult,
CompileResult,
Program(..),
Query(..),
Expression(..),
Constant(..),
Expr(..),
Definition(..),
Arg,
BinOp(..),
Op(..)
) where

import Data.Map (Map)
import qualified Data.Map as Map
import Instruction

type ID = String
type VariableSet = Map ID [Instruction]
type FunctionResult = [Instruction]

type CompileResult = ([FunctionResult], VariableSet)

data Expression = EExpr Expr
    | EConstant Constant
    | EProgram Program
    | EQuery Query
    | EArgs [Arg]

-- | Program Structure
data Program = Program {
    programId :: ID,
    programExpr :: Expr, 
    programQueries :: [Query]
}

-- | Query Structure
data Query = Query {
    queryId :: ID,
    queryArgs :: [Arg],
    queryExprs :: [Expr]
}

-- | The expression type
data Expr = ExprConstant Constant
    | ExprDef [Definition] Expr
    | ExprObserve Expr Expr
    | ExprSample DistUserDefined
    | ExprDistrib Distribution
    | ExprReference ID
    | ExprFunctionCall ID [Expr]
    | ExprBinOp BinOp
    | ExprBracketing Expr

-- | Data type for a constant
data Constant = EInt Int
    | EFloat Float
    | EBoolean Bool
    | EPercentage Float

-- | Definition structure
data Definition = VarDef {varId :: ID, expr :: Expr}
    | FuncDef ID [Arg] [Expr]
    | DistDef ID [Arg] [Expr]

type Arg = ID

-- | Set of distributions
data Distribution = DistBernoulli Expr
    | DistBeta Expr Expr
    | DistNormal Expr Expr
    | DistFlip Expr
    | DistUserDefined [Expr]

data BinOp = BinOp {
    op :: Op,
    expr1 :: Expr,
    expr2 :: Expr
}

-- | Set of binary operations.
data Op = ADD
    | MULT
    | SUBTRACT
    | OVER
    | OR
    | AND
    | GREATER_THAN
    | LESS_THAN
    | EQUALS
