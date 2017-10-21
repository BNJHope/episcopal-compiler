module CommandHandler
( ProgramInfo,
handleInput
) where

import System.Environment

data ProgramInfo = ProgramInfo {
	filename :: String
	, args :: [String]
}

handleInput :: IO()
handleInput = do
	args <- getArgs
	
getProgramInfo :: [String] -> ProgramInfo
getProgramInfo xs = 

getArgsAndFilename :: [String] -> ([String], [String])
getArgsAndFilename args :: partition $ argIsFlag args

partitionTupleToProgramInfo :: ([String], [String]) -> ProgramInfo
partitionTupleToProgramInfo ([flags], [filenameList]) = ProgramInfo (head filenameList) flags

getFlagsFromArgs :: [String] -> [String]
getFlagsFromArgs args = filter argIsFlag args

getFilename :: [String] -> String
getFilename args = head $ filter (\arg -> not $ argIsFlag arg) args

argIsFlag :: String -> Bool
argIsFlag arg = take 1 arg == "-"
