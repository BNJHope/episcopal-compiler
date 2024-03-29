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
import Debug.Trace

type ClassName = String
type VariableAddress = Int
type StackLimit = Int
type LocalVariableLimit = Int

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
    where ((compiledExpr:otherFuncs), classes, _) = compileExprs exprs $ combineVars (createVarSet args 0) vars

-- | Compile the main method and return
-- | a list of functions where the first function
-- | is the main method and the remaining functions
-- | are functions that were found and compiled while
-- | processing the main.
compileMainMethod :: [Expr] -> VariableSet -> [FunctionResult]
compileMainMethod exprs vars =
    [[getMainMethodHeader]
    ++ [getStackLimit 20]
    ++ [getLocalsLimit 20]
    ++ [importPrintStreamInstruction]
    ++ mainFunc
    ++ [printObject]
    ++ [getVoidReturn]
    ++ [getEndMethod]
    ++ [getNewLine]]
    ++ otherFuncs
    where
        ((mainFunc:otherFuncs), classes , _) = compileExprs exprs vars

-- | Compile a list of expressions with the given set of
-- | variables.
compileExprs :: [Expr] -> VariableSet -> CompileResult
compileExprs exprs vars = foldr compileExprFoldable ([], [], vars) exprs

-- | The function to apply during the process of folding over
-- | the expressions. It determines how to update the overall
-- | compilation result depending on the result compiling the
-- | next expression.
compileExprFoldable :: Expr -> CompileResult -> CompileResult
compileExprFoldable expr (funcs, classes, vars) =
    constructCompileResult (funcs, classes, vars) newCompileResult
    where newCompileResult = compileExpr expr vars

-- | Construct a new compile result a previous compile result version
-- | and a new compilation result to be appended.
constructCompileResult :: CompileResult -> CompileResult -> CompileResult
constructCompileResult (oldFuncs, oldClasses, oldVars) (newFuncs, newClasses, newVars)
    = (updateFuncs oldFuncs newFuncs, oldClasses ++ newClasses, combineVars oldVars newVars)

-- | Update the list of functions, given a set of old functions
-- | and a new set of functions to add to the list.
updateFuncs :: [FunctionResult] -> [FunctionResult] -> [FunctionResult]
updateFuncs [] [] = []
updateFuncs [] newFuncs = newFuncs
updateFuncs oldFuncs [] = oldFuncs

-- The first element in each list is a fragment for the same function
-- so we combine those two lists together. Any additional functions
-- are appended to the end of the list.
updateFuncs (oldTopFunc:oldExtraFuncs) (newTopFunc:newExtraFuncs)
    = [oldTopFunc ++ newTopFunc] ++ oldExtraFuncs ++ newExtraFuncs

-- | Compile an expression type
compileExpr :: Expr -> VariableSet -> CompileResult
compileExpr (ExprConstant const) vars = ([compileConstant const], [] , vars)

-- Compile any definitions and then compile the expression part
-- given the definitions that have just been compiled.
compileExpr (ExprDef defs nextExpr) vars =
    -- Compile the definitions
    let (newFuncs, defClasses, newVars) = compileDefinitions defs vars
    
    -- Compile the expression given the new definitions. The returned variables
    -- are not used so we can disregard them. We split the returned functions from
    -- head, as the head element is the main expression that has been evaluated.
        ((expressionToReturn:extraFunctions), exprClasses, _) = compileExpr nextExpr newVars
    
    -- Return the expression with any extra functions and classes that were defined
    -- that will need to be defined later. Return the original set of variables so that
    -- scope is not leaked.
    in ([expressionToReturn] ++ newFuncs ++ extraFunctions, defClasses ++ exprClasses, vars)

compileExpr (ExprFunctionCall id exprs) vars = ((compileMethodCall id exprs vars), [], vars)
compileExpr (ExprBinOp binop) vars = compileBinOp binop vars
compileExpr (ExprBracketing expr) vars = compileExpr expr vars
compileExpr (ExprReference id) vars = ([vars Map.! id], [], vars)
compileExpr (ExprDistrib dist) vars = compileDistribution dist vars
compileExpr (ExprSample expr) vars = compileSample expr vars
compileExpr (ExprObserve validity expr) vars = compileObservation validity expr vars

-- | Compile an observation expression.
compileObservation :: Expr -> Expr -> VariableSet -> CompileResult
compileObservation validityCheck exprToExecute vars
    -- Get instructions to check if expression is valid
    -- Get execute branch for when it is invalid
    -- Get execute branch for when it is valid
    = ([
        -- Expression that determines validity
        validityCheckExpr

        -- Convert the float object on the top of the stack
        -- into an int so that it can be compared.
        ++ getIntFromFloatObj

        -- Check if the expression resolves to valid.
        ++ [getObservationCheck]

        -- Get the set of instructions to carry out if invalid.
        ++ getInvalidObersationInstrs

        -- Get goto to jump over expression to be executed if it failed.
        ++ [getGoToEndObservationLabel]

        -- Get the observation execution label to jump to.
        ++ [getObservationExecuteLabel]

        -- Get a new observation result to use.
        ++ getNewObservationResult

        -- Get the evaluated expression.
        ++ execExpression

        -- Initialise the observation result
        -- with the result.
        ++ [getInitTrueObservationResult]

        -- Get the end label for the invalid statement execution
        -- to jump to.
        ++ [getObservationEndLabel]]
        ++ otherValidityFuncs ++ otherExecFuncs, validityClasses ++ execClasses, vars)
    where
        ((validityCheckExpr:otherValidityFuncs), validityClasses, _) = compileExpr validityCheck vars
        ((execExpression:otherExecFuncs), execClasses, _) = compileExpr exprToExecute vars

-- | Get the set of instructions
-- | to be executed after an invalid observation
-- | expression.
getInvalidObersationInstrs :: [Instruction]
getInvalidObersationInstrs = getNewObservationResult
                            ++ [getInitFalseObservationResult]

-- | Load a new observation result object
-- | onto the stack.
getNewObservationResult :: [Instruction]
getNewObservationResult = ["new ObservationResult"] ++ ["dup"]

-- | Init an observation result object
-- | with default parameters (i.e validity set to false)
getInitFalseObservationResult :: Instruction
getInitFalseObservationResult = "invokespecial ObservationResult/<init>()V"

-- | Init an observation result object
getInitTrueObservationResult :: Instruction
getInitTrueObservationResult = "invokespecial ObservationResult/<init>(Ljava/lang/Float;)V"

-- | Compile a call to sample a distribution.
compileSample :: Expr -> VariableSet -> CompileResult
-- load the distribution and then 
compileSample expr vars = ([dist ++ [invokeSample]] ++ otherFuncs, classes, vars)
    where (dist:otherFuncs, classes, _) = compileExpr expr vars

-- | Compile a call to get an instance of a distribution.
compileDistribution :: Distribution -> VariableSet -> CompileResult

-- Compile Bernoulli call
compileDistribution (DistBernoulli expr) vars
    = getDistributionCompileResult (getNewDist "Bernoulli") invokeBernoulliInit [expr] vars

-- Compile Flip call
compileDistribution (DistFlip expr) vars
    = getDistributionCompileResult (getNewDist "Flip") invokeFlipInit [expr] vars
    
-- Compile Beta call
compileDistribution (DistBeta expr1 expr2) vars
    = getDistributionCompileResult (getNewDist "Beta") invokeBetaInit [expr1, expr2] vars
    
-- Compile Normal call
compileDistribution (DistNormal expr1 expr2) vars
    = getDistributionCompileResult (getNewDist "Normal") invokeNormalInit [expr1, expr2] vars

-- | Get the result from compiling a distribution, given its instructions
-- | for making a new object and also its instructions for carrying out its
-- | <init> method.
getDistributionCompileResult :: [Instruction] -> Instruction -> [Expr] -> VariableSet -> CompileResult
getDistributionCompileResult new init exprs vars
    = ([new ++ compiledExpr ++ [init]] ++ otherFuncs, classes, vars)
    where ((compiledExpr:otherFuncs), classes, _) = compileExprs exprs vars

-- | Instructions for a new Bernoulli distribution.
getNewDist :: Instruction -> [Instruction]
getNewDist distName =  ["new " ++ distName] ++ ["dup"]

-- | Instructions for invoking the init of the Bernoulli.
invokeBernoulliInit :: Instruction
invokeBernoulliInit = "invokespecial Bernoulli/<init>(Ljava/lang/Float;)V"

-- | Instructions for invoking the init of the Flip.
invokeFlipInit :: Instruction
invokeFlipInit = "invokespecial Flip/<init>(Ljava/lang/Float;)V"

-- | Instructions for invoking the init of the Bernoulli.
invokeBetaInit :: Instruction
invokeBetaInit = "invokespecial Beta/<init>(Ljava/lang/Float;Ljava/lang/Float;)V"

-- | Instructions for invoking the init of the Flip.
invokeNormalInit :: Instruction
invokeNormalInit = "invokespecial Normal/<init>(Ljava/lang/Float;Ljava/lang/Float;)V"

-- | Invoke the interface sample method on a distribution.
invokeSample :: Instruction
invokeSample = "invokeinterface IDistribution/sample()Ljava/lang/Float; 1"

-- | Compile query
compileQuery :: Query -> VariableSet -> [FunctionResult]
compileQuery (Query queryId queryArgs queryExprs) vars
    = compileMethod queryId queryArgs queryExprs vars

-- | Compile the instructions for when a method is called.
compileMethodCall :: ID -> [Expr] -> VariableSet -> [FunctionResult]
compileMethodCall methodId exprs vars = 

        -- Load the arguments onto the stack.
        [getArgLoadInstructions(take (length exprs) compiledExprs)

        -- Get the invoke method call.
        ++ [
        "invokestatic "
        ++ classname
        ++ "/" ++ methodId
        ++ "(" ++ (foldr (\arg argsSig -> argsSig ++ "Ljava/lang/Float;") "" exprs)
        ++ ")" ++ "Ljava/lang/Float;" ]]

        -- Add any additional expressions, such as new functions, into the set
        -- of functions to be returned.
        ++ drop (length exprs) compiledExprs
        where (classname:_) = vars Map.! "__classname__"
              (compiledExprs, classes, newVars) = compileExprs exprs vars

-- | Combine the given compiled expressions which will load
-- | the arguments to be passed to a function call onto the stack.
getArgLoadInstructions :: [FunctionResult] -> [Instruction]
getArgLoadInstructions exprs
    = foldr (\expressionInstrs instrsList -> instrsList ++ expressionInstrs) [] exprs

-- | Compile a constant
compileConstant :: Constant -> [Instruction]
compileConstant (EInt val) = compileInt val
compileConstant (EFloat val) = compileFloat val
compileConstant (EBoolean val) = compileBool val
compileConstant (EPercentage val) = compilePercentage val

-- | Compile a set of definitions.
compileDefinitions :: [Definition] -> VariableSet -> CompileResult
compileDefinitions defs vars = foldr (compileDefsFoldable vars) ([], [], Map.empty) defs

-- | The foldable function to be applied between compilation results while
-- | compiling a list of definitions.
compileDefsFoldable :: VariableSet -> Definition -> CompileResult -> CompileResult
compileDefsFoldable vars def cumulativeCompileResult =
    constructCompileResult cumulativeCompileResult newCompileResult
    where newCompileResult = compileDefinition def vars

-- | Compile a set of variable and function definitions.
compileDefinition :: Definition -> VariableSet -> CompileResult

-- Compile a variable definition - return an updated variables list to be used for a sub expression
-- in the compile result. Return a list of extra function definitions with an empty list at the start
-- as there was no more expression code used and only extra functions.
compileDefinition (VarDef varId varExpr) vars = ([] ++ extraFuncs, classDefs, Map.insert varId definition newVars)
    where ((definition:extraFuncs), classDefs , newVars) = compileExpr varExpr vars

-- Compile a function definition.
compileDefinition (FuncDef id args exprs) vars = (funcs, [], vars)
    where funcs = compileMethod id args exprs vars

-- | Compile the given binary operation.
compileBinOp :: BinOp -> VariableSet -> CompileResult

compileBinOp (BinOp ADD expr1 expr2) vars
    = getBinOpCompilationResult expr1 expr2 ["fadd"] vars

compileBinOp (BinOp MULT expr1 expr2) vars
    = getBinOpCompilationResult expr1 expr2 ["fmul"] vars

compileBinOp (BinOp SUBTRACT expr1 expr2) vars
    = getBinOpCompilationResult expr1 expr2 ["fsub"] vars

compileBinOp (BinOp OVER expr1 expr2) vars
    = getBinOpCompilationResult expr1 expr2 ["fdiv"] vars

compileBinOp (BinOp OR expr1 expr2) vars
    = getBinOpAndOrEqCompilationResult expr1 expr2 ["ior"] vars

compileBinOp (BinOp AND expr1 expr2) vars
    = getBinOpAndOrEqCompilationResult expr1 expr2 ["iand"] vars

compileBinOp (BinOp GREATER_THAN expr1 expr2) vars
    = getBinOpCompilationResult expr1 expr2 (["fcmpg"] ++ [invokeIntToFloat]) vars

compileBinOp (BinOp LESS_THAN expr1 expr2) vars
    = getBinOpCompilationResult expr1 expr2 (["fcmpl"] ++ [invokeIntToFloat]) vars

compileBinOp (BinOp EQUALS expr1 expr2) vars
    = getBinOpAndOrEqCompilationResult expr1 expr2 getBinOpEqualityResultFunc vars

-- | Get the compilation result from performing a
-- | binary operation on two expressions.
getBinOpCompilationResult :: Expr -> Expr -> [Instruction] -> VariableSet -> CompileResult
getBinOpCompilationResult expr1 expr2 opInstrs vars
    = ( [expr1Result
        ++ [invokeFloatValueOf]
        ++ expr2Result
        ++ [invokeFloatValueOf]
        ++ opInstrs
        ++ [invokeCreateFloatObject]]
        ++ expr1ExtraFuncs
        ++ expr2ExtraFuncs, newClasses1 ++ newClasses2, vars)
    where ((expr1Result:expr1ExtraFuncs), newClasses1, expr1NewVars) = compileExpr expr1 vars
          ((expr2Result:expr2ExtraFuncs), newClasses2, expr2NewVars) = compileExpr expr2 vars

-- | Get the compilation result from
-- | performing a binary operation on the stack where
-- | the results of the two expressions need to be converted
-- | into int values before the operation is carried out.
getBinOpAndOrEqCompilationResult :: Expr -> Expr -> [Instruction] -> VariableSet -> CompileResult
getBinOpAndOrEqCompilationResult expr1 expr2 opInstrs vars
    = ( [expr1Result
        ++ [invokeFloatValueOf]
        ++ [invokeFloatToInt]
        ++ expr2Result
        ++ [invokeFloatValueOf]
        ++ [invokeFloatToInt]
        ++ opInstrs
        ++ [invokeIntToFloat]
        ++ [invokeCreateFloatObject]]
        ++ expr1ExtraFuncs
        ++ expr2ExtraFuncs, newClasses1 ++ newClasses2, vars)
    where ((expr1Result:expr1ExtraFuncs), newClasses1, expr1NewVars) = compileExpr expr1 vars
          ((expr2Result:expr2ExtraFuncs), newClasses2, expr2NewVars) = compileExpr expr2 vars

-- | Get the set of instructions
-- | that compare an equality. If the
-- | values are equal, the result loads
-- | 1 onto the stack. If there are not
-- | equal, it loads 0 onto the stack.
getBinOpEqualityResultFunc :: [Instruction]
getBinOpEqualityResultFunc = 
    ["ifeq Equality"]
    ++ ["ldc 0"]
    ++ ["goto EndEquality"]
    ++ ["Equality:"]
    ++ ["ldc 1"]
    ++ ["EndEquality:"]

-- | Get the header of a method.
getMethodHeader :: ID -> [Arg] -> Instruction
getMethodHeader id args = ".method public static " ++ id ++ "(" ++ argsSymbs ++ ")" ++ getReturnSymb
    where argsSymbs = getArgsSymbs args

-- | Compile an integer value
compileInt :: Int -> [Instruction]
compileInt val = ["ldc " ++ show val] ++ ["i2f"] ++ [invokeCreateFloatObject]

-- | Compile a float
compileFloat :: Float -> [Instruction]
compileFloat val = ["ldc " ++ show val] ++ [invokeCreateFloatObject]

-- | Compile a boolean
compileBool :: Bool -> [Instruction]
compileBool False = ["ldc " ++ show 0] ++ ["i2f"] ++ [invokeCreateFloatObject]
compileBool True = ["ldc " ++ show 1] ++ ["i2f"] ++ [invokeCreateFloatObject]

-- | Compile a percentage value
compilePercentage :: Float -> [Instruction]
compilePercentage val = ["ldc " ++ show val]

-- | Create a variable set - maps given
-- | argument names to memory fetch values.
createVarSet :: [Arg] -> Int -> VariableSet
createVarSet [] _ = Map.empty
createVarSet (nextArg:remainingArgs) argIndex =
    Map.insert nextArg [(getArgumentLoadInstruction argIndex)] nextVarSet
    where nextVarSet = createVarSet remainingArgs (argIndex + 1)

-- | Combine a set of variables.
combineVars :: VariableSet -> VariableSet -> VariableSet
combineVars vars1 vars2 = Map.union vars1 vars2

-- | Get the instruction for loading an argument
-- | at the given location.
getArgumentLoadInstruction :: Int -> Instruction
getArgumentLoadInstruction index = "aload " ++ show index

-- | Load the variable at the given address onto the stack
loadAddressFromVariableOntoStack :: VariableAddress -> Instruction
loadAddressFromVariableOntoStack address = "aload_" ++ show address

-- | Get the preamble for defining a class in Jasmin.
getClassPreamble :: ClassName -> [Instruction]
getClassPreamble className = [getClassnameDefinitionLine className]
    ++ [getSuperConstructorLine]

-- | Get the init method for a class.
getInitMethod :: [Instruction]
getInitMethod = [getInitMethodHeader]
    ++ [loadAddressFromVariableOntoStack 0]
    ++ [invokeObjectInit]
    ++ [getVoidReturn]
    ++ [getEndMethod]
    ++ [getNewLine]

-- | Get the instruction which causes the observation to be checked.
getObservationCheck :: Instruction
getObservationCheck = "ifgt PassedObservation"

-- | Get the label for where valid observations should start
-- | executing.
getObservationExecuteLabel :: Instruction
getObservationExecuteLabel = "PassedObservation:"

-- | Get the label for the failed observation to jump
-- | to after loading its message onto the stack.
getGoToEndObservationLabel :: Instruction
getGoToEndObservationLabel = "goto EndObservation"

-- | Get the label to jump to when
-- | the observation statement has finished.
getObservationEndLabel :: Instruction
getObservationEndLabel = "EndObservation:"

-- | Get the line for defining the given class name.
getClassnameDefinitionLine :: ClassName -> Instruction
getClassnameDefinitionLine className = ".class public " ++ className

-- | Get the super constructor instruction.
getSuperConstructorLine :: Instruction
getSuperConstructorLine = ".super java/lang/Object"

-- | Get the header for the init method.
getInitMethodHeader :: Instruction
getInitMethodHeader =  ".method public <init>()V"

-- | Get the line for invoking the init method.
invokeObjectInit :: Instruction
invokeObjectInit = "invokenonvirtual java/lang/Object/<init>()V"

-- | Get the instruction to invoke the sample method.
invokeSampleMethod :: ID -> Instruction
invokeSampleMethod distrId = "invokevirtual " ++ distrId ++ "/sample()Ljava/lang/Float;"

-- | Get the instruction to create a float object from
-- | the first float on the stack.
invokeCreateFloatObject :: Instruction
invokeCreateFloatObject = "invokestatic java/lang/Float/valueOf(F)Ljava/lang/Float;"

-- | Get the instruction to invoke the valueOf method
-- | of a float object.
invokeFloatValueOf :: Instruction
invokeFloatValueOf = "invokevirtual java/lang/Float/floatValue()F"

-- Get the integer representation of a float object.
getIntFromFloatObj :: [Instruction]
getIntFromFloatObj = [invokeFloatValueOf] ++ [invokeFloatToInt]

-- | Invoke the function for converting a float primitive
-- | to an int primivite.
invokeFloatToInt :: Instruction
invokeFloatToInt = "f2i"

-- | Invoke the function for converting an int
-- | primitive into a float primitive.
invokeIntToFloat :: Instruction
invokeIntToFloat = "i2f"

-- | Get the header line for the main method.
getMainMethodHeader :: Instruction
getMainMethodHeader = ".method public static main([Ljava/lang/String;)V"

-- | Import the dists library
importDists :: Instruction
importDists =  "invokenonvirtual ./lib/Bernoulli/<init>(Ljava/lang/Float;)V;"

-- | Get the symbol representation of all of the arguments in a method call.
getArgsSymbs :: [Arg] -> Instruction
getArgsSymbs args = foldr (\arg symbs-> symbs ++ getArgSymb arg) [] args

-- | Get a return function symbol.
getReturnSymb :: Instruction
getReturnSymb = "Ljava/lang/Float;"

-- Get the symbol representation of an argument in a method call.
getArgSymb :: Arg -> Instruction
getArgSymb arg = "Ljava/lang/Float;"

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
getFloatReturn = "areturn"

-- | Determines the end of a method definition
getEndMethod :: Instruction
getEndMethod = ".end method"
