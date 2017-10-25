module Structures(
AST,
Constant(..)
) where

type AST = Constant

-- | The expression type
data Expr = EConstant Constant

-- | Data type for a constant
data Constant = EInt Integer
    | EFloat Float
    | EBoolean Bool
    | EPercentage Float

