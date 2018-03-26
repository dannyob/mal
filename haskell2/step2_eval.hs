{-# LANGUAGE OverloadedStrings #-}

import System.IO
import Reader
import Printer
import MalType
import Eval
import System.Console.Readline
import Data.Maybe
import Env

-- import Data.Map

malREAD :: [String] -> Either String MalType
malREAD x = do
        rf <- read_form x
        return (fst $ rf)

malPRINT :: MalType -> String
malPRINT x = pr_str x True

rep :: [String] -> String
rep x = case malREAD x of
        Left err -> err
        Right mts -> malPRINT $ snd $ malEVAL (mkMalEnv repl_env) mts

repl = do
    minput <- readline "user> "
    addHistory $ fromMaybe "" minput
    tokens <- tokenizer $ fromMaybe "" minput
    if tokens /= [] then do
        putStrLn $ rep tokens
        hFlush stdout
    else return ()
    repl

main = repl
