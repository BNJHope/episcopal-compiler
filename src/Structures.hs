module Structures(
AST,
ID,
Program(..),
Query(..),
Expression(..),
Constant(..),
Expr(..),
Args
) where

import qualified Data.Map as Map

type ID = String
type VariableSet = Map ID Expr

type CompileResult = ([Instruction], VariableSet)

data Expression = EExpr Expr
    | EConstant Constant
    | EProgram Program
    | EQuery Query
    | EArgs Args

-- | Program Structure
data Program = Program {
    programId :: ID,
    programExpr :: Expr, 
    programQueries :: [Query]
}

-- | Query Structure
data Query = Query {
    queryId :: ID,
    queryArgs :: Args,
    queryExprs :: [Expr]
}

-- | The expression type
data Expr = ExprConstant Constant
    | ExprDef [Definition] Expr
    | ExprObserve Expr Expr
    | ExprSample Expr
    | ExprDistrib Distribution
    | ExprReference ID
    | ExprFunctionCall ID [Expr]
    | ExprBinOp Expr Op Expr
    | ExprBracketing Expr

-- | Data type for a constant
data Constant = EInt Int
    | EFloat Float
    | EBoolean Bool
    | EPercentage Float

-- | Definition structure
data Definition = FuncDef ID Args [Expr]
    | DistDef ID Args [Expr]

type Args = [ID]

-- | Set of distributions
data Distribution = DistBernoulli Expr
    | DistBeta Expr Expr
    | DistNormal Expr Expr
    | DistFlip Expr
    | DistUserDefined [Expr]

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
