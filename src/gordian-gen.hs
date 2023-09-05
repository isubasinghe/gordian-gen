{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Control.Applicative ((<|>))
import Data.Char (isDigit)
import Data.SCargot
import Data.SCargot.Repr.Basic
import Data.Text (Text, pack, unpack)
import Numeric (readHex)
import System.Environment (getArgs)

-- Our operators are going to represent addition, subtraction, or
-- multiplication
{- data Op = Add | Sub | Mul deriving (Eq, Show)

-- The atoms of our language are either one of the aforementioned
-- operators, or positive integers
data Atom = AOp Op | ANum Int deriving (Eq, Show) -}

-- Once parsed, our language will consist of the applications of
-- binary operators with literal integers at the leaves
-- data Expr = EOp Op Expr Expr | ENum Int deriving (Eq, Show)

-- Conversions to and from our Expr type
{- toExpr :: SExpr Atom -> Either String Expr
toExpr (A (AOp op) ::: l ::: r ::: Nil) = EOp op <$> toExpr l <*> toExpr r
toExpr (A (ANum n)) = pure (ENum n)
toExpr sexpr = Left ("Unable to parse expression: " ++ show sexpr)

fromExpr :: Expr -> SExpr Atom
fromExpr (EOp op l r) = A (AOp op) ::: fromExpr l ::: fromExpr r ::: Nil
fromExpr (ENum n) = A (ANum n)
 -}
-- Parser and serializer for our Atom type

{- sAtom :: Atom -> Text
sAtom (AOp Add) = "+"
sAtom (AOp Sub) = "-"
sAtom (AOp Mul) = "*"
sAtom (ANum n) = pack (show n) -}

-- Our final s-expression parser and printer:

{- -- mkLangPrinter :: SExprPrinter Atom Expr
mkLangPrinter =
  setFromCarrier fromExpr $
    setIndentStrategy (const Align) $
      basicPrint sAtom
 -}
main = do
  putStrLn "DONE"
