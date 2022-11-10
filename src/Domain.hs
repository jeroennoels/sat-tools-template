{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}

module Domain (Domain(..), mkDomain) where

import Data.Set (Set)
import qualified Data.Set as Set
import Util (mkSet)


class Domain dom elm | dom -> elm where
  cardinality :: dom -> Int
  contains :: dom -> [elm] -> Bool
  encode :: dom -> elm -> Int
  decode :: dom -> Int -> elm

instance Ord a => Domain (Set a) a where
  cardinality = Set.size
  contains s xs = Set.isSubsetOf (Set.fromList xs) s
  encode s x = Set.findIndex x s
  decode s i = Set.elemAt i s

mkDomain :: Ord a => [a] -> Set a
mkDomain = mkSet
