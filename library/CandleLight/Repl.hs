{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}

module CandleLight.Repl where

import CandleLight.Lang.Expr
import CandleLight.Parser
import CandleLight.Lang.Builtin (defaultCtx)
import CandleLight.Lang.Eval
import Control.Monad.State.Strict
import Control.Monad.Except
import qualified Data.Map as Map
import qualified Data.List as List

import System.Console.Repline
import qualified System.Console.Haskeline.MonadException as HMonadExcept

import Text.Parsec

type Repl a = HaskelineT (StateT Context (ExceptT Error IO)) a

instance (HMonadExcept.MonadException m) => HMonadExcept.MonadException (ExceptT e m) where
  controlIO f = ExceptT $ HMonadExcept.controlIO $ \(HMonadExcept.RunIO run) -> let
    run' = HMonadExcept.RunIO (fmap  ExceptT . run . runExceptT)
    in fmap runExceptT $ f run'

data ReplSettings = ReplSettings {
    theCtx :: Context
  , promptStr :: String
}

cmd :: String -> Repl ()
cmd input = do
  case (parse exprParser "" input) of
    (Left s) -> liftIO (print s)
    (Right e) -> let
      strEvald = (fmap show $ eval e) `catchError` (pure . errorOut)
      in
      lift (strEvald >>= liftIO . putStrLn)

comp :: (Monad m, MonadState Context m) => WordCompleter m
comp n = do
  ns <- fmap Map.keys $ get
  return $ filter (List.isPrefixOf n) ns

helpRepl :: [String] -> Repl ()
helpRepl args = liftIO $ print $ "Help" ++ (show args)

quit :: [String] -> Repl ()
quit _ = abort

opts :: [(String, [String] -> Repl ())]
opts = [
        ("help", helpRepl),
        ("quit", quit),
        ("q",quit)
       ]


defaultReplSettings :: ReplSettings
defaultReplSettings = ReplSettings {
    theCtx = defaultCtx
  , promptStr = "λ "
}

ini :: Repl ()
ini = pure ()

replWithSettings :: ReplSettings -> IO ()
replWithSettings settings = (runExceptT $ flip evalStateT (theCtx settings) $
   evalRepl (promptStr settings) cmd opts (Word comp) ini) >>= either (print . errorOut) pure


repl :: IO ()
repl = replWithSettings defaultReplSettings
