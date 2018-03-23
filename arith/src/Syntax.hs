module Syntax where

import Data.Semigroup ((<>))
import Data.Set (Set)
import qualified Data.Set as Set

data TmBool = TmTrue | TmFalse
  deriving (Eq, Show)

data Lit
  = LBool TmBool
  | LZero
  deriving (Eq, Show)

instance Ord Lit where
  (LBool TmFalse) <= LZero = True
  (LBool TmTrue)  <= LZero = False
  (LBool TmFalse) <= (LBool TmTrue)   = True
  (LBool TmTrue)  <= (LBool TmFalse) = False
  LZero <= (LBool TmTrue)  = True
  LZero <= (LBool TmFalse) = True

data Term
  = TmLit Lit
  | TmCond Term Term Term
  | TmSucc Term
  | TmPred Term
  | TmTest Term
  deriving (Eq, Show)

consts :: Term -> Set Lit
consts (TmLit l) = Set.singleton l
consts (TmCond t1 t2 t3) = consts t1 <> consts t2 <> consts t3
consts (TmSucc t) = consts t
consts (TmPred t) = consts t
consts (TmTest t) = consts t

size :: Term -> Int
size (TmLit l) = 1
size (TmCond t1 t2 t3) = 1 + size t1 + size t2 + size t3
size (TmSucc t) = 1 + size t
size (TmPred t) = 1 + size t
size (TmTest t) = 1 + size t

depth :: Term -> Int
depth (TmLit l) = 1
depth (TmCond t1 t2 t3) = 1 + maximum [depth t1, depth t2, depth t3]
depth (TmSucc t) = 1 + depth t
depth (TmPred t) = 1 + depth t
depth (TmTest t) = 1 + depth t
