module Identifier where

class Identifier i where
  numId :: Int -> i
  nameId :: String -> i
  nestId :: [i] -> i
