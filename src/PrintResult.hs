module PrintResult (
getPrintResultInstructions
) where

import Structures
import EpiscopalResult
import Instruction

getPrintResultInstructions :: Result -> [Instruction]
getPrintResultInstructions (ConstantResult res) = [importPrintStreamInstruction]
    ++ [loadConstantOntoStack res]
	++ [convertIntConstantToString]
	++ [invokePrint]

importPrintStreamInstruction :: Instruction
importPrintStreamInstruction = "getstatic java/lang/System/out Ljava/io/PrintStream;"

loadConstantOntoStack :: Constant -> Instruction
loadConstantOntoStack (EInt int) | (int < 128) && (int > -129) = "bipush " ++ show int
    | (int < 32768) && (int > -32769) = "sipush " ++ show int
	| otherwise = "ldc " ++ show int

convertIntConstantToString :: Instruction
convertIntConstantToString = "invokestatic java/lang/String/valueOf(I)Ljava/lang/String;"

invokePrint :: Instruction
invokePrint = "invokevirtual java/io/PrintStream/println(Ljava/lang/String;)V"
