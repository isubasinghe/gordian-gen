{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE QuasiQuotes #-}

module EAST where


import GHC.Generics

data BV
data B

data Expr a where 
  Add :: Expr BV -> Expr BV -> Expr BV 
  Sub :: Expr BV -> Expr BV -> Expr BV
  And :: Expr B -> Expr B -> Expr B


data AST where
  Expr :: Expr a -> AST
  Func :: [Expr a] -> AST


data MyMaybe a = MyNothing | MyJust a
  deriving (Generic)


data Tag 
    = A Bool 
    | B BV 
    | C Bool
    deriving (Generic)

preFunc :: MyMaybe Int -> Tag -> AST
preFunc m t = [fn| 
  caseWhen m {
    MyJust(val) -> {
     caseWhen t {
      A(val) -> val && true
      B(val) -> val | 0xb01
      C(val) -> val || false
     } 
    }
    MyNothing -> 4 
  }|]

