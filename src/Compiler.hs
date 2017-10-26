module Compiler
( compile
) where

import Instruction
import Structures
import ProgramInfo
import PrintResult
import EpiscopalResult

type ClassName = String
type VariableAddress = Int
type StackLimit = Int
type LocalVariableLimit = Int

compile :: Expression -> [Instruction]
compile (EProgram prog) = compileProgram prog
compile (EConstant const) = compileConstant const

-- | Compile a program structure
compileProgram :: Program -> [Instruction]
compileProgram (Program programId programExpr programQueries)
    = getFilePreamble programId
    ++ compileExpr programExpr
    ++ foldr (\query instructionList -> instructionList ++ compileQuery query) [] programQueries
    ++ [getVoidReturn]
    ++ [getEndMethod]

-- | Compile a constant
compileConstant :: Constant -> [Instruction]
compileConstant (EInt val) = compileInt val
compileConstant (EFloat val) = compileFloat val
compileConstant (EBoolean val) = compileBool val
compileConstant (EPercentage val) = compilePercentage val

-- | Compile an expression type
compileExpr :: Expr -> [Instruction]
compileExpr (ExprConstant const) = compileConstant const 

-- | Compile query
compileQuery :: Query -> [Instruction]
compileQuery (Query queryId queryArgs queryExprs)
    = []

-- | Compile an integer value
compileInt :: Int -> [Instruction]
compileInt val | (val < 128) && (val > -129) = ["bipush " ++ show val]
    | (val < 32768) && (val > -32769) = ["sipush " ++ show val]
    | otherwise = ["ldc " ++ show val]

-- | Compile a float
compileFloat :: Float -> [Instruction]
compileFloat val = ["ldc " ++ show val]

-- | Compile a boolean
compileBool :: Bool -> [Instruction]
compileBool False = ["bipush " ++ show 0]
compileBool True = ["bipush " ++ show 1]

-- | Compile a percentage value
compilePercentage :: Float -> [Instruction]
compilePercentage val = ["fconst_" ++ show val]

-- | Load the variable at the given address onto the stack
loadAddressFromVariableOntoStack :: VariableAddress -> Instruction
loadAddressFromVariableOntoStack address = "aload_" ++ show address

getFilePreamble :: ClassName -> [Instruction]
getFilePreamble className = getClassPreamble className
    ++ getInitMethod
    ++ getMainMethod

getClassPreamble :: ClassName -> [Instruction]
getClassPreamble className = [getClassnameDefinitionLine className]
    ++ [getSuperConstructorLine]

getInitMethod :: [Instruction]
getInitMethod = [getInitMethodHeader]
    ++ [loadAddressFromVariableOntoStack 0]
    ++ [invokeObjectInit]
    ++ [getVoidReturn]
    ++ [getEndMethod]

getMainMethod :: [Instruction]
getMainMethod = [getMainMethodHeader]
    ++ [getStackLimit 20]
    ++ [getLocalsLimit 20]

getClassnameDefinitionLine :: ClassName -> Instruction
getClassnameDefinitionLine className = ".class public " ++ className

getSuperConstructorLine :: Instruction
getSuperConstructorLine = ".super java/lang/Object"

getInitMethodHeader :: Instruction
getInitMethodHeader =  ".method public <init>()V"

invokeObjectInit :: Instruction
invokeObjectInit = "invokenonvirtual java/lang/Object/<init>()V"

getMainMethodHeader :: Instruction
getMainMethodHeader = ".method public static main([Ljava/lang/String;)V"

getStackLimit :: StackLimit -> Instruction
getStackLimit lim = ".limit stack " ++ show lim

getLocalsLimit :: LocalVariableLimit -> Instruction
getLocalsLimit lim = ".limit locals " ++ show lim

getVoidReturn :: Instruction
getVoidReturn = "return"

getEndMethod :: Instruction
getEndMethod = ".end method"


