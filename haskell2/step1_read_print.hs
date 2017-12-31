{-# LANGUAGE OverloadedStrings #-}

import System.IO

malREAD :: String -> String
malREAD x = x

malEVAL :: String -> String
malEVAL x = x

malPRINT :: String -> String
malPRINT x = x

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
