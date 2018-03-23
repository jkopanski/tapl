module Parser where

import Prelude hiding (succ, pred)
import Control.Applicative
import qualified Data.HashSet as HashSet
import Data.Text
import Text.Parser.Char
import Text.Parser.Token
import Text.Parser.Token.Highlight

import Syntax

(<$!>) :: Monad m => (a -> b) -> m a -> m b
f <$!> ma = do
  a <- ma
  pure (f $! a)

arithStyle :: TokenParsing m => IdentifierStyle m
arithStyle = IdentifierStyle
  { _styleName = "arith"
  , _styleStart = letter
  , _styleLetter = letter
  , _styleReserved = HashSet.fromList ["ture", "false", "0", "succ", "pred", "if", "then", "else"]
  , _styleHighlight = Identifier
  , _styleReservedHighlight = ReservedIdentifier
  }

reserved :: (Monad m, TokenParsing m) => Text -> m ()
reserved = token . reserveText arithStyle

bool :: (Monad m, TokenParsing m) => m TmBool
bool = reserved "true"  *> pure TmTrue
   <|> reserved "false" *> pure TmFalse

literal :: (Monad m, TokenParsing m) => m Lit
literal = LBool <$!> bool
      <|> reserved "0" *> pure LZero

term, cond, succ, pred, test, lit :: (Monad m, TokenParsing m) => m Term
cond = TmCond <$!> (reserved "if"   *> term)
               <*> (reserved "then" *> term)
               <*> (reserved "else" *> term)
succ = TmSucc <$!> (reserved "succ"   *> term)
pred = TmPred <$!> (reserved "pred"   *> term)
test = TmTest <$!> (reserved "iszero" *> term)
lit = TmLit <$!> literal

term = lit
   <|> parens (cond <|> succ <|> pred <|> test)
