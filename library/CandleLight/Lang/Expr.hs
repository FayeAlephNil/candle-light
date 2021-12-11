module CandleLight.Lang.Expr where

import Data.Map (Map)
import Data.Maybe
import Control.Monad.State.Strict
import Control.Monad.Except

type Context = Map String Expr

data Error = ArgError String
           | Err Expr
           | CtxError String String Context

type Eval a = StateT Context (ExceptT Error IO) a

getCtx :: Eval Context
getCtx = get

data Literal = LitInt Integer
             | LitFloat Double
             | LitChar Char
             | LitBool Bool deriving (Eq)

data Expr = Atom String
          | Ident String 
          | ExprList [Expr] 
          | ExprFn String ([Expr] -> Eval Expr)
          | ExprMacro String ([Expr] -> Eval Expr)
          | Module String Context
          | Lit Literal

makeStr :: String -> Expr
makeStr = ExprList .  fmap (Lit . LitChar)

pureFn :: String -> ([Expr] -> Expr) -> Expr
pureFn s f = ExprFn s (pure . f)

nil :: Expr
nil = ExprList []

exprChar :: Expr -> Maybe Char
exprChar (Lit (LitChar c)) = Just c
exprChar _ = Nothing

isInt :: Expr -> Bool
isInt (Lit (LitInt _)) = True
isInt _ = False

isFloat :: Expr -> Bool
isFloat (Lit (LitFloat _)) = True
isFloat _ = False

isIdent :: Expr -> Bool
isIdent (Ident _) = True
isIdent _ = False

instance Show Literal where
    show (LitInt i) = show i
    show (LitFloat d) = show d
    show (LitChar c) = show c
    show (LitBool b) = show b

instance Show Expr where
    show (Atom s) = '.' : s
    show (Ident s) = s
    show (ExprList es) = let
        defaultShow =  "(" ++ unwords (fmap show es) ++ ")"
        in fromMaybe defaultShow $ traverse exprChar es
    show (ExprFn s _) = "<function:" ++ s ++ ">"
    show (ExprMacro s _) = "<macro:" ++ s ++ ">"
    show (Module name ctx) = "Module (" ++ name ++ "): " ++ show ctx
    show (Lit l) = show l
