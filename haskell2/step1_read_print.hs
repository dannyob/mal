{-# LANGUAGE OverloadedStrings #-}

import System.IO
import Reader
import Printer
import MalType

malREAD :: String -> MalType
malREAD x = fst $ read_form $ tokenizer x

malEVAL :: MalType -> MalType
malEVAL x = x

malPRINT :: MalType -> String
malPRINT x = pr_str x True

rep :: String -> String
rep x = malPRINT $ malEVAL $ malREAD x

repl = do
    putStr "user> "
    hFlush stdout
    input <- getLine
    putStrLn $ rep input
    hFlush stdout
    repl

main = repl
