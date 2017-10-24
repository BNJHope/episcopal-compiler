module Structures(
AST,
Constant(..)
) where

type AST = Constant

data Constant = EInt Integer
    | EFloat Float
    | EBoolean Bool
    | EPercentage Float

