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

type AST = Expression
type ID = String

data Expression = EExpr Expr
    | EConstant Constant
    | EProgram Program
    | EQuery Query
    | EArgs Args

data Program = Program {
programId :: ID,
programExpr :: Expr, 
programQueries :: [Query]
}

data Query = Query {
queryId :: ID,
queryArgs :: Args,
queryExprs :: [Expr]
}

-- | The expression type
data Expr = ExprConstant Constant

-- | Data type for a constant
data Constant = EInt Int
    | EFloat Float
    | EBoolean Bool
    | EPercentage Float

type Args = [ID]
