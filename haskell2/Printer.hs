module Printer (pr_str) where

import MalType

escape "" = ""
escape (x:xs)
    | x == '\n' = "\\n" ++ escape xs
    | x == '\\' = "\\\\" ++ escape xs
    | x == '"' = "\\\"" ++ escape xs
    | otherwise = x:(escape xs)

pr_list :: [MalType] -> Bool -> String
pr_list [] _ = ""
pr_list (x:xs) print_readably
    | xs == [] = pr_str x print_readably
    | otherwise = (pr_str x print_readably) ++ " " ++ (pr_list xs print_readably)

pr_str :: MalType -> Bool -> String
pr_str MalNil _ = "nil"
pr_str MalTrue _ = "true"
pr_str MalFalse _ = "false"
pr_str (MalNumber n) _ = show n
pr_str (MalSymbol s) _ = s
pr_str (MalKeyword s) _ = ":" ++ s
pr_str (MalString s) True = "\"" ++ (escape s) ++ "\""
pr_str (MalString s) False = "\"" ++ s ++ "\""  -- I think maybe this isn't
                                                -- supposed to have quotes
                                                -- around it in this case?
pr_str (MalList x) print_readably = "(" ++ (pr_list x print_readably) ++ ")"
pr_str (MalVector x) print_readably = "[" ++ (pr_list x print_readably) ++ "]"
pr_str (MalBuiltinFunction x) _ = "<builtin function>"
