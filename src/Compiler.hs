module Compiler
( compile
) where

import Instruction
import Structures
import ProgramInfo
import PrintResult
import EpiscopalResult

compile :: AST -> [Instruction]
compile ast = getPrintResultInstructions $ ConstantResult $ EInt 42
