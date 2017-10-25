module Structures(
AST,
Expression(..),
Constant(..),
Expr(..)
) where

type AST = Expression

data Expression = EExpr Expr
    | EConstant Constant

-- | The expression type
data Expr = ExprConstant Constant

-- | Data type for a constant
data Constant = EInt Integer
    | EFloat Float
    | EBoolean Bool
    | EPercentage Float

