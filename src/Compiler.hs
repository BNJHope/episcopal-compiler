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
    ++ getInitMethod
    ++ foldr (\func programInstrs -> programInstrs ++ func) [] funcs
    where funcs = 
        compileFunctionSet (programExpr prog) (programQueries prog) env
        env = (Map.fromList [("__classname__", programId prog)])

-- | Compiles the set of functions kept by the class
compileFunctionSet :: Expr -> [Query] -> VariableSet -> [FunctionResult]
compileFunctionSet expr [] vars =
    -- When there are no queries, only compile the main method.
    compileMainMethod [expr] vars
compileFunctionSet expr queries vars =
    -- Main method from initial expression
    compileMainMethod [expr] vars
    -- Remaining functions from the queries
	++ foldr (\query funcs -> funcs ++ (compileMethod (queryId query) (queryArgs query) (queryExprs query))) [] queries

-- | Compile a method and return a list of functions
-- | where the first function is the method that has been
-- | compiled and the rest are methods defined within the
-- | method.
compileMethod :: ID -> [Arg] -> [Expr] -> [FunctionResult]
compileMethod id args exprs =
    -- Get method header
    [getMethodHeader id arg
	++ getStackLimit 20
	++ getLocalsLimit 20
    ++ compiledExpr
    ++ getFloatReturn]
    ++ otherFuncs
    where ((compiledExpr:otherFuncs), _) = compileExprs exprs newVars
        newVars = createVarSet args

compileMainMethod :: [Expr] -> VariableSet -> [FunctionResult]
compileMainMethod exprs vars =
    [[getMainMethodHeader]
	++ getStackLimit 20
	++ getLocalsLimit 20
	++ mainFunc]
	++ otherFuncs
    where
        ((mainFunc:otherFuncs), _) = foldr compileExprFoldable [] exprs
		
compileExprs :: [Expr] -> VariableSet -> CompileResult
compileExpr exprs vars = foldr compileExprFoldable [] exprs

compileExprFoldable :: Expr -> VariableSet -> CompileResult -> CompileResult
compileExprFoldable expr vars ([topFunc:otherFuncs], varsSoFar) =
	([[topFunc ++ newTopFuncInstrs] ++ otherFuncs ++ newOtherFuncs], combineVars varsSoFar newVars)
	where ([newTopFuncInstrs:newOtherFuncs], newVars) = compileExpr expr vars

-- | Compile an expression type
compileExpr :: Expr -> VariableSet -> CompileResult
compileExpr (ExprConstant const) vars = (compileConstant const, vars)
compileExpr (ExprDef defs nextExpr) vars =
    let (newFuncs, newVars) = compileDefinitions defs vars
    in ((compileExpr nextExpr newVars) ++ newFuncs, vars)
compileExpr (ExprReference id) vars = (vars ! id, vars)
compileExpr (ExprFunctionCall id exprs) vars = ([compileMethodCall id exprs vars], vars)
compileExpr (ExprBinOp op expr1 expr2) vars = (compileBinOp op expr1 expr2 vars, vars)
compileExpr (ExprBracketing expr) = compileExpr expr

-- | Compile query
compileQuery :: Query -> [Instruction]
compileQuery (Query queryId queryArgs queryExprs)
    = compileMethod queryId queryArgs queryExprs

-- | Compile the instructions for when another method is called.
compileMethodCall :: ID -> [Expr] -> VariableSet -> [Instruction]
compileMethodCall methodId exprs vars = [
		-- Compile set of expressions before function call
		-- to load everything onto the stack
		-- foldr (\expr instrs -> instrs ++ f)
		"invokestatic "
		++ (vars ! "__classname__")
		++ "/(" ++ methodId
		++ (foldr (\arg argsSig -> argsSig ++ "F") "" exprs)
		++ ")" + "F" ]

-- | Compile a constant
compileConstant :: Constant -> [Instruction]
compileConstant (EInt val) = compileInt val
compileConstant (EFloat val) = compileFloat val
compileConstant (EBoolean val) = compileBool val
compileConstant (EPercentage val) = compilePercentage val

-- | Compile a set of definitions.
compileDefinitions :: [Definitions] -> VariableSet -> CompileResult
-- Iterate over the definitions and merge new functions in first tuple segment and
-- merge variables in the second tuple segment
-- compileDefinitions defs vars = foldr (\def compResult -> (fst compResult ++ fst result, )
--		where ) [] defs

-- | Compile a set of variable and function definitions.
compileDefinition :: Definition -> VariableSet -> CompileResult
compileDefinition (VarDef var) vars = ([] ++ extraFuncs, Map.insert id definition newVars)
    where id = id var
          ((definition:extraFuncs) , newVars) = compileExpr (expr var) vars

compileDefinitions (FuncDef id args exprs) vars = (funcDef ++ extraFuncs, vars)
	where ((funcDef:extraFuncs), newVars) = compileMethod id args exprs ID

compileBinOp :: ExprBinOp -> VariableSet -> CompileResult
compileBinOp (ADD expr1 expr2) vars = ( [expr1Result ++ expr2Result ++ ["fadd"]] ++ expr1ExtraFuncs ++ expr2ExtraFuncs, vars)
	where ((expr1Result:expr1ExtraFuncs), expr1NewVars) = compileExpr expr1 vars
		((expr2Result:expr2ExtraFuncs), expr2NewVars) = compileExpr expr2 vars
 
-- | Get the header of a method.
getMethodHeader :: ID -> [Arg] -> Instruction
getMethodHeader id args = ".method public static " + id + "(" + argsSymbs ")" + returnSymb
    where argsSymbs = getArgsSymbs args
        returnSymb = getReturnSymb

-- | Compile an integer value
compileInt :: Int -> [Instruction]
compileInt val = ["ldc " ++ show val]
{-compileInt val | (val < 128) && (val > -129) = ["bipush " ++ show val]
    | (val < 32768) && (val > -32769) = ["sipush " ++ show val]
    | otherwise = ["ldc " ++ show val]-}

-- | Compile a float
compileFloat :: Float -> [Instruction]
compileFloat val = ["ldc " ++ show val]

-- | Compile a boolean
compileBool :: Bool -> [Instruction]
compileBool False = ["ldc " ++ show 0]
compileBool True = ["ldc " ++ show 1]

-- | Compile a percentage value
compilePercentage :: Float -> [Instruction]
compilePercentage val = ["ldc " ++ show val]

-- | Load the variable at the given address onto the stack
loadAddressFromVariableOntoStack :: VariableAddress -> Instruction
loadAddressFromVariableOntoStack address = "aload_" ++ show address

getClassPreamble :: ClassName -> [Instruction]
getClassPreamble className = [getClassnameDefinitionLine className]
    ++ [getSuperConstructorLine]

getInitMethod :: [Instruction]
getInitMethod = [getInitMethodHeader]
    ++ [loadAddressFromVariableOntoStack 0]
    ++ [invokeObjectInit]
    ++ [getVoidReturn]
    ++ [getEndMethod]

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

getReturnSymb :: Instruction
getReturnSymb = "F"

-- Get the symbol representation of an argument in a method call.
getArgSymb :: Arg -> Instruction
getArgSymb arg var = "F"

-- | Get the limit of the size of the stack
getStackLimit :: StackLimit -> Instruction
getStackLimit lim = ".limit stack " ++ show lim

-- | Get the limit of local variables for a method.
getLocalsLimit :: LocalVariableLimit -> Instruction
getLocalsLimit lim = ".limit locals " ++ show lim

-- | Return on void.
getVoidReturn :: Instruction
getVoidReturn = "return"

-- | Return a float.
getFloatReturn :: Instruction
getFloatReturn = "freturn"

-- | Determines the end of a method definition
getEndMethod :: Instruction
getEndMethod = ".end method"
