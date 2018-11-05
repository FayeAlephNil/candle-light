module CandleLight.Exec where 


import CandleLight.Parser
import CandleLight.Lang.Eval
import CandleLight.Lang.Builtin
import Text.Parsec


exec :: String -> IO ()
exec content = execAll [content]

execAll :: [String] -> IO ()
execAll contents = case traverse (parse allParser "") contents of 
    (Left s) -> print s
    (Right es) -> execJustErrors defaultCtx (concat es)
