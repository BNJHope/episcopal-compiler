module EpisCompilable (
	EpisCompilable
) where

import Instruction

class EpisCompilable a where
    compile :: a -> [Instruction]


