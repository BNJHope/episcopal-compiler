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

compile :: AST -> [Instruction]
compile ast = getFilePreamble "Episcopal"
    ++ getPrintResultInstructions (ConstantResult (EInt 42))
    ++ [getVoidReturn]
	++ [getEndMethod]

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


