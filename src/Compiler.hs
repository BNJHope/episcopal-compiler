module Compiler
( compile,
compileExpr
) where

import Data.Map (Map)
import qualified Data.Map as Map
import Instruction
import Structures
import ProgramInfo
import PrintResult
import EpiscopalResult
import Debug.Trace

type ClassName = String
type VariableAddress = Int
type StackLimit = Int
type LocalVariableLimit = Int

{-
compile :: Expression -> [Instruction]
compile (EProgram prog) = compileProgram prog
compile (EConstant const) = compileConstant const
-}

-- | Compile a program structure
compile :: Program -> [Instruction]
compile prog
    = getClassPreamble (programId prog)
    ++ getInitMethod
    ++ foldr (\func programInstrs -> programInstrs ++ func) [] funcs
    where funcs = compileFunctionSet (programExpr prog) (programQueries prog) (Map.fromList [("__classname__", [programId prog])])

-- | Compiles the set of functions kept by the class
compileFunctionSet :: Expr -> [Query] -> VariableSet -> [FunctionResult]
compileFunctionSet expr [] vars =
    -- When there are no queries, only compile the main method.
    compileMainMethod [expr] vars
compileFunctionSet expr queries vars =
    -- Main method from initial expression
    compileMainMethod [expr] vars
    -- Remaining functions from the queries
    ++ foldr (\query funcs -> funcs ++ (compileMethod (queryId query) (queryArgs query) (queryExprs query) vars)) [] queries

-- | Compile a method and return a list of functions
-- | where the first function is the method that has been
-- | compiled and the rest are methods defined within the
-- | method.
compileMethod :: ID -> [Arg] -> [Expr] -> VariableSet -> [FunctionResult]
compileMethod id args exprs vars =
    -- Get method header
    [[getMethodHeader id args]
    ++ [getStackLimit 20]
    ++ [getLocalsLimit 20]
    ++ compiledExpr
    ++ [getFloatReturn]
    ++ [getEndMethod]
    ++ [getNewLine]]
    ++ otherFuncs
    where ((compiledExpr:otherFuncs), _) = compileExprs exprs $ combineVars (createVarSet args 0) vars

compileMainMethod :: [Expr] -> VariableSet -> [FunctionResult]
compileMainMethod exprs vars =
    [[getMainMethodHeader]
    ++ [getStackLimit 20]
    ++ [getLocalsLimit 20]
    ++ [importPrintStreamInstruction]
    ++ mainFunc
    ++ printFloat
    ++ [getVoidReturn]
    ++ [getEndMethod]
    ++ [getNewLine]]
    ++ otherFuncs
    where
        ((mainFunc:otherFuncs), _) = compileExprs exprs vars

compileExprs :: [Expr] -> VariableSet -> CompileResult
-- compileExprs exprs vars = trace ("Compiling exprs\nVars: " ++ (show vars)) (compileExprs' exprs vars)
compileExprs exprs vars = foldr compileExprFoldable ([], vars) exprs

compileExprFoldable :: Expr -> CompileResult -> CompileResult
compileExprFoldable expr ([], oldVars) = 
    constructExprCompileResult ([], oldVars) (newFuncs, newVars)
        where (newFuncs, newVars) = compileExpr expr oldVars

compileExprFoldable expr (funcs, vars) =
    constructExprCompileResult (funcs, vars) (newFuncs, newVars)
    where (newFuncs, newVars) = compileExpr expr vars

constructExprCompileResult :: CompileResult -> CompileResult -> CompileResult
constructExprCompileResult ([], oldVars) ([], newVars) = ([], combineVars oldVars newVars)
constructExprCompileResult ([], oldVars) (newFuncs, newVars)
    = (newFuncs, combineVars oldVars newVars)
constructExprCompileResult (oldFuncs, oldVars) ([], newVars)
    = (oldFuncs, combineVars oldVars newVars)
constructExprCompileResult ((oldTopFunc:oldExtraFuncs), oldVars) ((newTopFunc:newExtraFuncs), newVars)
    = ([oldTopFunc ++ newTopFunc] ++ oldExtraFuncs ++ newExtraFuncs, combineVars oldVars newVars)

-- | Compile an expression type
compileExpr :: Expr -> VariableSet -> CompileResult
compileExpr (ExprConstant const) vars = ([compileConstant const], vars)
compileExpr (ExprDef defs nextExpr) vars =
    let (newFuncs, newVars) = compileDefinitions defs vars
        ((restOfExpression:extraFunctions), _) = compileExpr nextExpr newVars
    in ([restOfExpression] ++ newFuncs ++ extraFunctions, vars)
-- compileExpr (ExprReference id) vars = trace ("Compiling expr reference\nID : " ++ id ++ "\nVars : " ++ show vars) (compileExpr' (ExprReference id) vars)

compileExpr (ExprFunctionCall id exprs) vars = ([compileMethodCall id exprs vars], vars)
compileExpr (ExprBinOp binop) vars = compileBinOp binop vars
compileExpr (ExprBracketing expr) vars = compileExpr expr vars

compileExpr (ExprReference id) vars = ([vars Map.! id], vars)

-- | Compile query
compileQuery :: Query -> VariableSet -> [FunctionResult]
compileQuery (Query queryId queryArgs queryExprs) vars
    = compileMethod queryId queryArgs queryExprs vars

-- | Compile the instructions for when another method is called.
compileMethodCall :: ID -> [Expr] -> VariableSet -> [Instruction]
compileMethodCall methodId exprs vars = 
        combineFuncs(take (length exprs) compiledExprs)
        ++ [
        -- Compile set of expressions before function call
        -- to load everything onto the stack
        -- foldr (\expr instrs -> instrs ++ f)
        "invokestatic "
        ++ classname
        ++ "/" ++ methodId
        ++ "(" ++ (foldr (\arg argsSig -> argsSig ++ "F") "" exprs)
        ++ ")" ++ "F" ]
        where (classname:_) = vars Map.! "__classname__"
              (compiledExprs, newVars) = compileExprs exprs vars

combineFuncs :: [FunctionResult] -> [Instruction]
combineFuncs funcs = foldr (\func instrsList -> instrsList ++ func) [] funcs

-- | Compile a constant
compileConstant :: Constant -> [Instruction]
compileConstant (EInt val) = compileInt val
compileConstant (EFloat val) = compileFloat val
compileConstant (EBoolean val) = compileBool val
compileConstant (EPercentage val) = compilePercentage val

-- | Compile a set of definitions.
compileDefinitions :: [Definition] -> VariableSet -> CompileResult
compileDefinitions defs vars = foldr (compileDefsFoldable vars) ([], Map.empty) defs

compileDefsFoldable :: VariableSet -> Definition -> CompileResult -> CompileResult
compileDefsFoldable vars def ([], resVariableSet)
    =([newFunc] ++ extraNewFuncs, combineVars resVariableSet newVars)
    where ((newFunc:extraNewFuncs), newVars) = compileDefinition def vars
compileDefsFoldable vars def ((mainFunc:extraFuncs), resVariableSet)
    = ([mainFunc ++ newFunc] ++ extraFuncs ++ extraNewFuncs, combineVars resVariableSet newVars)
    where ((newFunc:extraNewFuncs), newVars) = compileDefinition def vars

-- | Compile a set of variable and function definitions.
compileDefinition :: Definition -> VariableSet -> CompileResult
compileDefinition (VarDef varId varExpr) vars = ([] ++ extraFuncs, Map.insert varId definition newVars)
    where ((definition:extraFuncs) , newVars) = compileExpr varExpr vars

compileDefinition (FuncDef id args exprs) vars = (funcs, vars)
    where funcs = compileMethod id args exprs vars

compileBinOp :: BinOp -> VariableSet -> CompileResult
compileBinOp (BinOp ADD expr1 expr2) vars = ( [expr1Result ++ expr2Result ++ ["fadd"]] ++ expr1ExtraFuncs ++ expr2ExtraFuncs, vars)
    where ((expr1Result:expr1ExtraFuncs), expr1NewVars) = compileExpr expr1 vars
          ((expr2Result:expr2ExtraFuncs), expr2NewVars) = compileExpr expr2 vars
 
-- | Get the header of a method.
getMethodHeader :: ID -> [Arg] -> Instruction
getMethodHeader id args = ".method public static " ++ id ++ "(" ++ argsSymbs ++ ")" ++ getReturnSymb
    where argsSymbs = getArgsSymbs args

-- | Compile an integer value
compileInt :: Int -> [Instruction]
compileInt val = ["ldc " ++ show val] ++ ["i2f"]
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

createVarSet :: [Arg] -> Int -> VariableSet
createVarSet [] _ = Map.empty
createVarSet (nextArg:remainingArgs) argIndex =
    Map.insert nextArg [(getArgumentLoadInstruction argIndex)] nextVarSet
    where nextVarSet = createVarSet remainingArgs (argIndex + 1)

combineVars :: VariableSet -> VariableSet -> VariableSet
combineVars vars1 vars2 = Map.union vars1 vars2

getArgumentLoadInstruction :: Int -> Instruction
getArgumentLoadInstruction index = "fload " ++ show index

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
    ++ [getNewLine]

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
getReturnSymb = ['F']

-- Get the symbol representation of an argument in a method call.
getArgSymb :: Arg -> Instruction
getArgSymb arg = ['F']

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
