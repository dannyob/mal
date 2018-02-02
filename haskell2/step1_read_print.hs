{-# LANGUAGE OverloadedStrings #-}

import System.IO
import Reader
import Printer
import MalType

malREAD :: [String] -> MalType
malREAD x = fst $ read_form x

malEVAL :: MalType -> MalType
malEVAL x = x

malPRINT :: MalType -> String
malPRINT x = pr_str x True

rep :: [String] -> String
rep x = malPRINT $ malEVAL $ malREAD x

repl = do
    putStr "user> "
    hFlush stdout
    input <- getLine
    tokens <- tokenizer input
    if tokens /= [] then do
        putStrLn $ rep tokens
        hFlush stdout
    else return ()
    repl

main = repl
