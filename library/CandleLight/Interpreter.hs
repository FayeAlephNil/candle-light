module CandleLight.Interpreter where

import CandleLight.Exec

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

interpret :: FilePath -> IO ()
interpret filename = do
    content <- readFile filename
    baseContents <- getTheBase
    execAll (baseContents ++ [content])