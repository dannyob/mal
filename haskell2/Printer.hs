module Printer (pr_str) where

import MalType

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
pr_str (MalString s) = "\"" ++ s ++ "\""    -- XXX: This is not correct,
                                            --      because of escaping.
                                            --      (We still need to add \
                                            --      before special characters.)
pr_str (MalList x) = "(" ++ (pr_list x) ++ ")"
pr_str (MalVector x) = "[" ++ (pr_list x) ++ "]"
