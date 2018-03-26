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

rep :: MalEnv -> [String] -> (MalEnv, String)
rep env x = case malREAD x of
        Left err -> (env, err)
        Right mts -> let (e, o) = malEVAL env mts in
                     (e, malPRINT o)

repl environ = do
    minput <- readline "user> "
    addHistory $ fromMaybe "" minput
    tokens <- tokenizer $ fromMaybe "" minput
    let (new_environ, output) = if tokens /= [] then rep environ tokens else (environ, "")
    putStr output
    putStr "\n"
    hFlush stdout
    repl new_environ

main = repl (mkMalEnv repl_env)
