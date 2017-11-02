module PrintResult (
getPrintResultInstructions,
printFloat,
importPrintStreamInstruction,
getNewLine,
getComment
) where

import Structures
import EpiscopalResult
import Instruction

getPrintResultInstructions :: Result -> [Instruction]
getPrintResultInstructions (ConstantResult res) = [importPrintStreamInstruction]
    ++ [loadConstantOntoStack res]
    ++ [convertConstantToString res]
    ++ [invokePrint]

printFloat :: [Instruction]
printFloat = [convertFloatToString]
        ++ [invokePrint]

importPrintStreamInstruction :: Instruction
importPrintStreamInstruction = "getstatic java/lang/System/out Ljava/io/PrintStream;"

getNewLine :: Instruction
getNewLine = ""

getComment :: Instruction -> Instruction
getComment comm = ";; " ++ comm

loadConstantOntoStack :: Constant -> Instruction
loadConstantOntoStack (EInt int) | (int < 128) && (int > -129) = "bipush " ++ show int
    | (int < 32768) && (int > -32769) = "sipush " ++ show int
    | otherwise = "ldc " ++ show int

loadConstantOntoStack (EFloat float) = "ldc " ++ show float

convertConsantToString :: Constant -> Instruction
convertConsantToString (EInt _) = "invokestatic java/lang/String/valueOf(I)Ljava/lang/String;"
convertConstantToString (EFloat _) = "invokestatic java/lang/String/valueOf(F)Ljava/lang/String;"

convertFloatToString :: Instruction
convertFloatToString = "invokestatic java/lang/String/valueOf(F)Ljava/lang/String;"

invokePrint :: Instruction
invokePrint = "invokevirtual java/io/PrintStream/println(Ljava/lang/String;)V"
