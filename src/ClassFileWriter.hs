module ClassFileWriter (
	writeInstructionsToFile
) where

import System.IO
import Instruction
--
-- | Write a list of JVM instructions to the given file.
writeInstructionsToFile :: FilePath -> [Instruction] -> IO()
writeInstructionsToFile outputFile instrs = do
	withFile outputFile WriteMode (\handle -> do
		mapM_ (\instr -> hPutStrLn handle instr) instrs
		)
