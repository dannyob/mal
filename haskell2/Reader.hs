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
    

type Readsource = IO ([String])

tokenizer :: String -> Readsource
tokenizer line = tokenizer' (return [""]) line

tokenizer' :: Readsource -> String -> Readsource
tokenizer' sofar remaining = do 
case remaining of
    "" -> sofar
    _ -> do
            unwrapped_sofar <- sofar
            token_regexp <- extracted_token
            regexp_results <- TR.regexec token_regexp remaining
            case regexp_results of
                Left a -> undefined
                Right Nothing -> return []
                Right (Just (_, _ , after , [""])) -> tokenizer' (return (unwrapped_sofar)) after
                Right (Just (_, _ , after , results)) -> tokenizer' (return (unwrapped_sofar ++ results)) after
