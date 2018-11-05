-- It is generally a good idea to keep all your business logic in your library
-- and only use it in the executable. Doing so allows others to use what you
-- wrote in their libraries.

import CandleLight.Exec
import System.Environment
import System.FilePath
import System.Directory

import Paths_candle_light

getTheBase :: IO [String]
getTheBase = do
    baseDir <- getDataDir
    base <- getDirectoryContents baseDir
    let candles = filter ((== ".candle") . takeExtension) base
    let addDir s = baseDir ++ "/" ++ s
    traverse (readFile . addDir) candles

main :: IO ()
main = do
    [filename] <- getArgs
    content <- readFile filename
    baseContents <- getTheBase
    execAll (baseContents ++ [content])
    