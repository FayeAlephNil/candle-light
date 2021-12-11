{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}

module CandleLight.Lang.Builtin (defaultCtx) where

import CandleLight.Lang.Expr
import CandleLight.Lang.Eval

import Control.Monad.State.Strict
import Control.Monad.Except
import qualified Data.Map as Map
-- import Data.Map (Map, (!))

primCtx :: Context
primCtx = Map.fromList 
    [
        plus
        , minus
        , times
        , floatDiv
        , set
        , err
        , fn
        , defn
        , ifMacro
        , eq
        , ("nil", nil)
        , macro
        , defmacro
        , patList
        , cons
        , printIt
        , getInput
        , defmodule
        , clearCtx
    ]

primModule :: Expr
primModule = Module "prim" primCtx

defaultCtx :: Context
defaultCtx = Map.insert "prim" primModule primCtx

doInt :: (t -> Integer -> p) -> String -> Eval t -> Expr -> Eval p
doInt op _ a (Lit (LitInt i)) = fmap (`op` i) a
doInt _ n _ _ = throwError . ArgError $ n ++ " supplied something which is not a number"

doFloat :: (t -> Double -> p) -> String -> Eval t -> Expr -> Eval p    
doFloat op _ a (Lit (LitFloat f)) = fmap (`op` f) a
doFloat op _ a (Lit (LitInt i)) = fmap (`op` fromIntegral i) a
doFloat _ n _ _ = throwError . ArgError $ n ++ " supplied something which is not a number"

numOp :: (forall a. (Num a) => a -> a -> a) -> String -> (forall a. (Num a) => a) -> (String, Expr)
numOp op name unit = (name, ExprFn name $ \case 
    xs -> fmap Lit $ 
        if any isFloat xs 
            then fmap LitFloat (foldl (doFloat op name) (pure unit) xs) 
            else fmap LitInt (foldl (doInt op name) (pure unit) xs)) 

plus :: (String, Expr)
plus = numOp (+) "+" 0

minus :: (String, Expr)
minus = ("-", ExprFn "-" $ \case 
    (Lit (LitFloat x) : xs) -> Lit . LitFloat . (x -) <$> plusHelperFloat xs
    (Lit (LitInt x) : xs) -> Lit <$> 
        if any isFloat xs 
            then LitFloat . (fromIntegral x -) <$> plusHelperFloat xs
            else LitInt . (x -) <$> plusHelperInt xs
    _ -> throwError $ ArgError "- supplied something which is not a number")
    where
        plusHelperFloat = foldl (doFloat (+) "-") (pure 0)
        plusHelperInt = foldl (doInt (+) "-") (pure 0)


times :: (String, Expr)
times = numOp (*) "*" 1

floatDiv :: (String, Expr)
floatDiv = ("/", ExprFn "/" $ \case 
    (Lit (LitFloat x) : xs) -> Lit . LitFloat . (x /) <$> timesHelper xs
    (Lit (LitInt x) : xs) -> Lit . LitFloat . (fromIntegral x /) <$> timesHelper xs
    _ -> throwError $ ArgError "/ supplied something which is not a number")
        where
            timesHelper = foldl (doFloat (*) "/") (pure 1)

cons :: (String, Expr)
cons = ("cons", ExprFn "cons" $ \case
    [e, ExprList es] -> pure $ ExprList (e : es)
    _ -> throwError $ ArgError "cons not supplied an expression and a list")

set :: (String, Expr)
set = ("set", ExprMacro "set" $ \case
    [Ident str, expr] -> do
        e <- eval expr
        modify (Map.insert str e)
        pure nil
    _ -> throwError $ ArgError "set not supplied an identifier and an expression")

err :: (String, Expr)
err = ("err", ExprFn "err" $ \case
    [] -> throwError $ Err (Ident "Error")
    (x : _) -> throwError $ Err x)

evalInCtx :: Context -> Expr -> Eval Expr
evalInCtx ctx = lift . flip evalStateT ctx . eval

evalAllInCtx :: Context -> [Expr] -> Eval Expr
evalAllInCtx _ [] = pure nil
evalAllInCtx ctx (e : es) = lift . flip evalStateT ctx . fmap last . evalAll $ (e : es)

argError :: String -> [a] -> [a] -> Eval ()
argError name is args = if length is == length args
    then pure ()
    else throwError . ArgError $ name ++ " not given enough arguments, it requires " ++ show (length is) ++ " arguments and was given " ++ show (length args)

newContext :: [String] -> [Expr] -> Eval Context
newContext is es = do
    ctx <- get
    pure . foldr (.) id (zipWith Map.insert is es) $ ctx

fn :: (String, Expr)
fn = ("fn", ExprMacro "fn" $ \case
        (ExprList is : (e : es)) -> if all isIdent is 
            then pure . ExprFn "anonymous function" $ \args -> do
                _ <- argError "anonymous function" is args
                ctx <- newContext (fmap show is) args
                evalAllInCtx ctx (e : es)
            else noIdentName
        (Ident argsName : (e : es)) -> pure . ExprFn "anonymous function" $ \args -> do
            ctx <- newContext [argsName] [ExprList args]
            evalAllInCtx ctx (e : es)
        _ -> noIdentName)
        where
            noIdentName = throwError $ ArgError "fn not given identifier list or a definition"

macro :: (String, Expr)
macro = ("macro", ExprMacro "macro" $ \case
            (ExprList is : (e : es)) -> if all isIdent is
                then pure . ExprMacro "anonymous macro" $ \args -> do
                    _ <- argError "anonymous macro" is args
                    ctx <- newContext (fmap show is) args
                    toEval <- evalAllInCtx ctx (e : es)
                    eval toEval
                else noIdentName
            (Ident argsName : (e : es)) -> pure . ExprMacro " anonymous macro" $ \args -> do
                ctx <- newContext [argsName] [ExprList args]
                toEval <- evalAllInCtx ctx (e : es)
                eval toEval
            _ -> noIdentName)
            where
                noIdentName = throwError $ ArgError "macro not given identifier list or a definition"

defn :: (String, Expr)
defn = ("defn", ExprMacro "defn" $ \case
    (Ident name : xs@(ExprList is : (_ : _))) -> if all isIdent is 
        then do
            (ExprFn _ func) <- eval $ ExprList (Ident "fn" : xs)
            eval (ExprList [Ident "set", Ident name, ExprFn name func])
        else noIdentName
    (Ident name : xs@(Ident argName : _)) -> do
        (ExprFn _ func) <- eval $ ExprList (Ident "fn" : xs)
        eval (ExprList [Ident "set", Ident name, ExprFn name func])
    _ -> noIdentName)
    where
        noIdentName = throwError $ ArgError "defn not given either an identifier list, a name, or a definition"

defmacro :: (String, Expr)
defmacro = ("defmacro", ExprMacro "defmacro" $ \case
    (Ident name : xs@(ExprList is : (_ : _))) -> if all isIdent is 
        then do
            (ExprMacro _ mac) <- eval $ ExprList (Ident "macro" : xs)
            eval (ExprList [Ident "set", Ident name, ExprMacro name mac])
        else noIdentName
    _ -> noIdentName)
    where
        noIdentName = throwError $ ArgError "defmacro not given either an identifier list, a name, or a definition"


ifMacro :: (String, Expr)
ifMacro = ("if", ExprMacro "if" $ \case
    [condition, Ident "then", ePass, Ident "else", eFail] -> do
        b' <- eval condition
        case b' of
            (Lit (LitBool True)) -> eval ePass
            (Lit (LitBool False)) -> eval eFail
            _ -> throwError $ ArgError "if supplied an expression which does not evaluate to a boolean"
    _ -> throwError $ ArgError "if not supplied the correct syntax of (if boolexpr then (expr1 expr2...) else (expr1' expr2'...)")

argLit :: String -> Expr -> Eval Literal
argLit _ (Lit l) = pure l
argLit n (Ident name) = do
  ctx <- get
  expr <- lookupIn "" name ctx
  argLit n expr
argLit n _ = throwError . ArgError $ n ++ "supplied a non-literal"
     
eq :: (String, Expr)
eq = ("=", ExprFn "=" $ \case
    [] -> pure . Lit . LitBool $ True
    (expr : exprs) -> do 
        e <- argLit "=" expr
        es <- traverse (argLit "=") exprs
        pure . Lit . LitBool . all (== e) $ es)

patList :: (String, Expr)
patList = ("pat-list", ExprMacro "pat-list" $ \case
    [a, eNil, ExprList [Ident n, Ident ns], eCons] -> do
        elst <- eval a
        case elst of
            (ExprList []) -> eval eNil
            (ExprList (y : ys)) -> do
                ctx <- newContext [n, ns] [y, ExprList ys]
                evalInCtx ctx eCons
            _ -> throwError . ArgError $ "pat-list not supplied a list"
    _ -> throwError . ArgError $ "pat-list not supplied the correct list, nil case, binding, and cons case")

printIt :: (String, Expr)   
printIt = ("print", ExprFn "print" $ \args -> do 
    exprs <- traverse eval args
    _ <- liftIO $ traverse print exprs
    pure nil)

defmodule :: (String, Expr)
defmodule = ("defmodule", ExprMacro "defmodule" $ \case
    (Ident name : exprs) -> do
        result <- liftIO $ evalWith defaultCtx exprs
        case result of
            (Left e) -> throwError e
            (Right ctx) -> modify (Map.insert name $ Module name ctx) >> pure (Module name ctx)
    _ -> throwError $ ArgError "defmodule not provided with a name")

clearCtx :: (String, Expr)
clearCtx = ("clear-ctx", ExprFn "clear-ctx" $ \_ -> do
    put $ Map.fromList [("prim", primModule)]
    pure nil)

getInput :: (String, Expr)
getInput = ("get-line", ExprFn "get-line" $ \case
    [] -> liftIO $ fmap makeStr getLine
    (_:_) -> throwError $ ArgError "get-line supplied arguments")
