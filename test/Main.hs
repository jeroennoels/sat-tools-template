module Main (main) where

import Formula
import Eval
import Arbitraries
import Clauses
import Dimacs
import Util
import Tools

import Test.QuickCheck
import System.Environment (getArgs)
import Control.Applicative (liftA2)
import Data.Set (Set)
import qualified Data.Set as Set

example1 :: Formula Int
example1 = (Not (Var 1) `Or` Var 2) `Equiv` (Var 1 `Implies` Var 2)

example2 :: Formula Int
example2 = ((Not (Var 1) `And` Var 4) `Equiv` (Var 2 `Or` Var 1)) `Or` Var 3

example3 :: IO (Formula IntLabel)
example3 = generate (arbitrarySizedFormula 100)

tautology :: Ord i => Formula i -> Bool
tautology f = all (evaluate f) (allAssignments $ variables f)

(<-->) :: Ord i => Formula i -> Formula i -> Bool
(<-->) f g = all (equalOn f g) (allAssignments vars)
  where vars = variables f `Set.union` variables g

(<==>) :: Ord i => [Clause i] -> Formula i -> Bool
(<==>) cs f = all (equivalentOn cs f) (allAssignments $ variables f)

prop_elimImplication :: Formula IntLabel -> Bool
prop_elimImplication f = elimImplication f <--> f

prop_moveNotDown :: Formula IntLabel -> Bool
prop_moveNotDown f = moveNotDown (elimImplication f) <--> f

-- Slow because it deals with CNF before it is simplified.
prop_toCNF :: Formula IntLabel -> Bool
prop_toCNF f = toCNF f <--> f

prop_Clauses :: Formula IntLabel -> Bool
prop_Clauses f = formulaToClauses f <==> f

assert :: String -> Bool -> IO ()
assert msg ok = putStrLn $ msg ++ if ok then " -> OK" else error msg

runTests :: IO ()
runTests = sequence_ $
  map quickCheck [prop_elimImplication,
                  prop_moveNotDown,
                  prop_Clauses]

main :: IO ()
main = getArgs >>= run

readLinesFromFile :: FilePath -> IO [String]
readLinesFromFile file = lines `fmap` readFile file

loadVariables :: IO [Int]
loadVariables = readVariables `fmap` readLinesFromFile "out.dimacs"

loadMapping :: IO [(Int, Id)]
loadMapping = readMapping `fmap` readLinesFromFile "problem.cnf"

loadModel :: IO String
loadModel = fmap show $ liftA2 getModel loadVariables loadMapping

problem :: [Clause Id]
problem = concatMap formulaToClauses test

varFilter :: Id -> Bool
varFilter (Simple _) = True
varFilter _ = False

run :: [String] -> IO ()
-- Test suite
run ["test"] = runTests
-- Generate the file to feed into the SAT solver
run ["p"] = dimacsOutput varFilter problem
run ["i"] = loadMapping >>= print
-- For debugging
run ["s"] = loadVariables >>= print
-- Process the file "out.dimacs" that holds the output of the SAT solver
run ["m"] = loadModel >>= print
