module CommandHandler
( handleInput
) where

import System.Environment

handleInput :: IO()
handleInput = do
	args <- getArgs
	
getFlagsFromArgs :: [String] -> [String]
getFlagsFromArgs args = filter argIsFlag args

getFilename :: [String] -> String
getFilename args = head $ filter (\arg -> not $ argIsFlag arg) args

argIsFlag :: String -> Bool
argIsFlag arg = take 1 arg == "-"
