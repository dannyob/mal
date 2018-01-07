module Reader (tokenizer) where

import Text.Regex.PCRE.String as TR

tokenregexp = "[\\s,]*(~@|[\\[\\]{}()'`~^@]|\"(?:\\\\.|[^\\\\\"])*\"|;.*|[^\\s\\[\\]{}('\"`,;)]*)"

compiled_token :: IO (Either (TR.MatchOffset, String) Regex)
compiled_token = TR.compile (TR.CompOption 0) (TR.ExecOption 0) tokenregexp

extracted_token :: IO Regex
extracted_token =  do
    atoken <- compiled_token
    case atoken of
        Left a -> undefined
        Right r -> return r
    

tokenizer :: IO ([String]) -> String -> IO ([String])
tokenizer sofar remaining = do 
    unwrapped_sofar <- sofar
    my_token <- extracted_token
    regexp_results <- TR.regexec my_token remaining
    if (remaining == "") then sofar
    else 
        case regexp_results of
            Left a -> undefined
            Right Nothing -> return []
            Right (Just (before, after, _, results)) -> tokenizer (return (unwrapped_sofar ++ results)) after


