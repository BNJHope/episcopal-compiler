module Structures(
AST,
Constant(..),
Expression(..)
) where

type AST = Expression

-- | The expression type
data Expression = EConstant Constant

-- | Data type for a constant
data Constant = EInt Integer
    | EFloat Float
    | EBoolean Bool
    | EPercentage Float

