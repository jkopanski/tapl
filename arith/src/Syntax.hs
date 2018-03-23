module Syntax where

data TmBool = TmTrue | TmFalse
  deriving (Eq, Show)

data Lit
  = LBool TmBool
  | LZero
  deriving (Eq, Show)

data Term
  = TmLit Lit
  | TmCond Term Term Term
  | TmSucc Term
  | TmPred Term
  | TmTest Term
  deriving (Eq, Show)
