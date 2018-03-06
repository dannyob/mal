{-# LANGUAGE OverloadedStrings #-}

import System.IO
import Reader
import Printer
import MalType

malREAD :: [String] -> Either String MalType
malREAD x = do
        rf <- read_form x
        return (fst $ rf)

malEVAL :: MalType -> MalType
malEVAL x = x

malPRINT :: MalType -> String
malPRINT x = pr_str x True

rep :: [String] -> String
rep x = case malREAD x of
        Left err -> err
        Right mts -> malPRINT $ malEVAL $ mts

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
