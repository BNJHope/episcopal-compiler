module Compiler
( compile
) where

import Data.Map (Map)
import qualified Data.Map as Map
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
compileProgram prog
    = getClassPreamble (programId prog)
    ++ compileFunctionSet (programExpr prog) (programQueries prog) (Map.fromList [("__classname__", programId)])

-- | Compiles the set of functions kept by the class
compileFunctionSet :: Expr -> [Query] -> VariableSet -> [FunctionResult]
compileFunctionSet expr [] vars =
    -- Main method from initial expression
	compileMethod "main" [] [expr]
    -- Remaining functions from the queries
    -- compileExpr (programExpr prog)
compileFunctionSet expr queries vars =
	compileMethod "main" [] [expr]
	++ map compileMethod() queries

-- | Compile an expression type
compileExpr :: Expr -> VariableSet -> [FunctionResult]
compileExpr (ExprConstant const) vars = [(compileConstant const, vars)]
compileExpr (ExprDef defs nextExpr) vars =
    let (newVars, newFuncs) = compileDefinitions defs vars
    in (compileExpr nextExpr newVars) ++ newFuncs
compileExpr (ExprReference id) vars = [vars ! id]
compileExpr (ExprFunctionCall id exprs) vars = [compileMethodCall id exprs vars]

-- | Compile query
compileQuery :: Query -> [Instruction]
compileQuery (Query queryId queryArgs queryExprs)
    = compileMethod queryId queryArgs queryExprs

-- | Compile the instructions for when another method is called.
compileMethodCall :: ID -> [Expr] -> VariableSet -> [Instruction]
compileMethodCall methodId exprs vars = ++ 

-- | Compile a constant
compileConstant :: Constant -> [Instruction]
compileConstant (EInt val) = compileInt val
compileConstant (EFloat val) = compileFloat val
compileConstant (EBoolean val) = compileBool val
compileConstant (EPercentage val) = compilePercentage val

-- | Compile a set of variable and function definitions.
compileDefinitions :: [Definition] 
compileDefinition :: Definition -> VariableSet -> CompileResult
compileDefinition VarDef vars = ([], Map.insert id instrs vars)
    where id = id var
          instrs = compileExpr (expr var) vars

-- compileBinOp :: ExprBinOp -> VariableSet -> [Instruction]
-- compileBinOp (ADD expr1 expr2) vars = 

-- | Compile a method
compileMethod :: ID -> [Arg] -> [Expr] -> [FunctionResult]
compileMethod id args exprs =
    -- Get method header
    getMethodHeader id args
    -- Create variable set from the arguments

-- | Get the header of a method.
getMethodHeader :: ID -> [Arg] -> Instruction
getMethodHeader id args = ".method public static " + id + "(" + argsSymbs ")" + returnSymb
    where argsSymbs = getArgsSymbs args
        returnSymb = getReturnSymb

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

-- | Get the symbol representation of all of the arguments in a method call.
getArgsSymbs :: [Arg] -> Instruction
getArgsSymbs args = foldr (\arg symbs-> symbs ++ getArgSymb arg) [] args

-- Get the symbol representation of an argument in a method call.
getArgSymb :: Arg -> VariableSet -> Instruction
getArgsSymb args vars

-- | Get the limit of the size of the stack
getStackLimit :: StackLimit -> Instruction
getStackLimit lim = ".limit stack " ++ show lim

-- | Get the limit of local variables for a method.
getLocalsLimit :: LocalVariableLimit -> Instruction
getLocalsLimit lim = ".limit locals " ++ show lim

-- | Return on void.
getVoidReturn :: Instruction
getVoidReturn = "return"

-- | Determines the end of a method definition
getEndMethod :: Instruction
getEndMethod = ".end method"


