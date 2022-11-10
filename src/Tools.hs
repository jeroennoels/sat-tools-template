module Tools where

import Util
import Formula
import Clauses
import Identifier

data Id = Simple Int | Compound [Id] | Name String
        deriving (Eq, Ord, Read, Show)

test = [Var (Simple 1)]
