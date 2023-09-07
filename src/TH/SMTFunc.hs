{- ORMOLU_DISABLE -}
{-# LANGUAGE QuasiQuotes #-}
module TH.SMTFunc where
import Language.Haskell.TH
import Language.Haskell.TH.Syntax
import Language.Haskell.TH.Lift
import Language.Haskell.TH.Quote 
import EAST
import Text.Parsec
import Text.Parsec.Expr
import Text.Parsec.Language (haskellStyle)
import Text.Parsec.String
import Text.Parsec.Pos

-- newtype (BitVecRepr a) => SMTArg a = SMTArg a 

location' :: Q SourcePos
location' = aux <$> location
  where
    aux :: Loc -> SourcePos
    aux loc = uncurry (newPos (loc_filename loc)) (loc_start loc)


parseIO :: Parser a -> String -> IO a 
parseIO = undefined


data SMTFunc = SMTFunc

parseSMTFunc :: Parser SMTFunc
parseSMTFunc = undefined
topLevel :: Parser a -> Parser a
topLevel p = undefined *> p <* eof


{- preCond x = [smtfunc|
    func x = case x of 
              MyJust a -> a == 4 
              MyNothing -> 0
  |] -}

