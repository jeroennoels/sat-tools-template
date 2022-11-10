module Constraint where

import Domain
import Relation
import Formula
import Clauses
import Identifier
import Util (natToBools)

import Data.Map.Strict (Map, (!))
import qualified Data.Map.Strict as Map

-- The Int holds the word width (number of bits) needed to represent the domain.
data Representation dom i = Representation String Int dom [i]

getName :: Representation dom i => String
getName (Representation name _ _ _) = name

mkRepresentation :: (Identifier i, Domain dom a) =>
  String -> dom -> [i] -> Representation dom i
mkRepresentation name dom is
  | lo && hi = Representation name w dom is
  | otherwise = error "Representation"
  where
    w = length is
    n = cardinality dom
    lo = 2^(w-1) <= n
    hi = n < 2^w

data Assign i = Assign Int [(i,Bool)]

toAssign :: (Identifier i, Domain dom a) =>
  Representation dom i -> Int -> Assign i
toAssign (Representation _ width _ is) n = Assign n bits
  where
    bits = zip is (natToBools width n)

match :: Identifier i => Assign i -> Formula i
match (Assign _ bits) = conjunction (map toLiteral bits)
  where
    toLiteral (i, False) = Not (Var i)
    toLiteral (i, True) = Var i

introduceMatch :: Identifier i => String -> Assign i -> (i, Formula i)
introduceMatch domain word@(Assign n bits) = (i,f)
  where
    i = nestId [nameId domain, numId n]
    f = introduce i (match word)

data Instantiation as bs a b i = Instantiation
  (Relation as bs a b)
  (Representation as i)
  (Representation bs i)

data Appender i = Appender (Map (String, Int) i) [Clause i]

append :: (Identifier i) => Representation dom i -> [Int] -> Appender i -> Appender i
append rep xs (Appender map clauses) = 
  where
    name = getName rep
    process x = introduceMatch name (toAssign rep x)
    (is,fs) = unzip (map process xs)
    

instantiate :: (Identifier i, Domain as a, Domain bs b) =>
  Instantiation as bs a b i -> Appender i -> Appender i
instantiate (Instantiation rel repa repb) app = g (f app) 
  where
    pairs = encodings rel
    xs = map fst pairs
    ys = map snd pairs
    f = append repa xs
    g = append repb ys
  
-- constraint :: Identifier i => Relation as bs a b -> String -> [i] -> [Clause i]
-- constraint rel name is = undefined
   
