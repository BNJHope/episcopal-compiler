module CommandHandler
(
handleInput
) where

import System.Environment
import Compiler
import ClassFileWriter
import ProgramInfo
import TestAST
import Data.List
import Structures

-- Handle the program input.
handleInput :: IO()
handleInput = do
    mapM_ (\ast ->
        writeInstructionsToFile (getFilenameFromProgramStructure ast) $ compile ast) getTestASTs
    -- writeInstructionsToFile "episcopal.out" $ compile $ head $ getTestASTs

getFilenameFromProgramStructure :: Program -> String
getFilenameFromProgramStructure prog = "output_progs/" ++ (programId prog) ++ ".epscpl"

-- | Get the program info structure from the set of arguments.
getProgramInfo :: [String] -> ProgramInfo
getProgramInfo args = partitionTupleToProgramInfo $ getArgsAndFilename args 

getArgsAndFilename :: [String] -> ([String], [String])
getArgsAndFilename args = partition argIsFlag args

-- | Convert a tuple from the output of partition into a program info data structure.
partitionTupleToProgramInfo :: ([String], [String]) -> ProgramInfo
partitionTupleToProgramInfo (flags, filenameList) = ProgramInfo (head filenameList) flags

-- | Get the set of flags from the arguments.
getFlagsFromArgs :: [String] -> [String]
getFlagsFromArgs args = filter argIsFlag args

-- | Get the filename from the set of arguments.
getFilename :: [String] -> String
getFilename args = head $ filter (\arg -> not $ argIsFlag arg) args

-- | Determine if a given argument is a flag or not.
argIsFlag :: String -> Bool
argIsFlag arg = take 1 arg == "-"
