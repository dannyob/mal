module Reader () where

import Text.Regex.PCRE.String as TR

tokenregexp = "[\\s,]*(~@|[\\[\\]{}()'`~^@]|\"(?:\\\\.|[^\\\\\"])*\"|;.*|[^\\s\\[\\]{}('\"`,;)]*)"

compiled_token = TR.compile (TR.CompOption 0) (TR.ExecOption 0) tokenregexp

tokenizer :: String -> [String]
tokenizer aline = ["foo", "bar"]
