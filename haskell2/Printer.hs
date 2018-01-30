module Printer (pr_str) where

import MalType

unescape "" = ""
unescape (x:xs)
    | x == '\n' = "\\n" ++ unescape xs
    | x == '\\' = "\\\\" ++ unescape xs
    | x == '"' = "\\\"" ++ unescape xs
    | otherwise = x:(unescape xs)

pr_list :: [MalType] -> String
pr_list (x:xs)
    | xs == [] = pr_str x
    | otherwise = (pr_str x) ++ " " ++ (pr_list xs)

pr_str :: MalType -> String
pr_str MalNil = "nil"
pr_str MalTrue = "true"
pr_str MalFalse = "false"
pr_str (MalNumber n) = show n
pr_str (MalSymbol s) = s
pr_str (MalString s) = "\"" ++ (unescape s) ++ "\""
pr_str (MalList x) = "(" ++ (pr_list x) ++ ")"
pr_str (MalVector x) = "[" ++ (pr_list x) ++ "]"