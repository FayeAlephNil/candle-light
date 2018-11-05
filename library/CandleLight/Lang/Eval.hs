module CandleLight.Lang.Eval where

import CandleLight.Lang.Expr

import Control.Monad.State
import Control.Monad.Except
import qualified Data.Map as Map

eval :: Expr -> Eval Expr
eval (Ident i) = do
    ctx <- get
    lookupIn "top level" i ctx
eval (ExprList []) = pure $ ExprList []
eval (ExprList (x : xs)) = do
    e <- eval x
    case e of
        (ExprFn _ f) -> traverse eval xs >>= f
        (ExprMacro _ f) -> f xs >>= eval
        (Module name ctx) -> case xs of
            [] -> throwError . ArgError $ "Module " ++ name ++ " used on nothing"
            [Ident i] -> lookupIn name i ctx
            _ -> throwError . ArgError $ "Module " ++ name ++ " not supplied an identifier or supplied more than one"
        _ -> fmap ExprList <$> sequence $ fmap eval (x : xs)
eval e = pure e

lookupIn :: String -> String -> Context -> Eval Expr
lookupIn name i ctx = case Map.lookup i ctx of
    Nothing -> throwError $ CtxError name i ctx
    (Just e) -> eval e

evalAll :: [Expr] -> Eval [Expr]
evalAll = traverse eval

runWith:: Context -> [Expr] -> IO (Either Error ([Expr], Context))
runWith ctx exprs = runExceptT . runStateT (evalAll exprs) $ ctx

errorOut :: Error -> String
errorOut (ArgError s) = "Argument Error:\n    " ++ s
errorOut (Err e) = "Error thrown by user:\n    " ++ show e
errorOut (CtxError name s ctx) = s ++ " not found in the context (" ++ name ++ "): " ++ show ctx

execJustErrors :: Context -> [Expr] -> IO ()
execJustErrors ctx exprs = runWith ctx exprs >>= either (putStrLn . errorOut) (const $ pure ())

execWith :: Context -> [Expr] -> IO ()
execWith ctx exprs = runWith ctx exprs >>= either (putStrLn . errorOut) (print . fst)

evalWith :: Context -> [Expr] -> IO (Either Error Context)
evalWith ctx exprs = fmap snd <$> runWith ctx exprs

execLastWith :: Context -> [Expr] -> IO ()
execLastWith _ [] = putStrLn "No code inputted"
execLastWith ctx (e : es) = runWith ctx (e : es) >>= either (putStrLn . errorOut) (print . last . fst)