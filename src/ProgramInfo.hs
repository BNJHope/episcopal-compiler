module ProgramInfo
(
ProgramInfo(..)
) where

data ProgramInfo = ProgramInfo {
	filename :: String
	, args :: [String]
}
