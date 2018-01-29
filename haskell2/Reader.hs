module Reader (tokenizer) where

import Text.Regex.PCRE.String as TR
import MalType

tokenregexp = "[\\s,]*(~@|[\\[\\]{}()'`~^@]|\"(?:\\\\.|[^\\\\\"])*\"|;.*|[^\\s\\[\\]{}('\"`,;)]*)"

compiled_token :: IO (Either (TR.MatchOffset, String) Regex)
compiled_token = TR.compile (TR.CompOption 0) (TR.ExecOption 0) tokenregexp

extracted_token :: IO Regex
extracted_token =  do
    atoken <- compiled_token
    case atoken of
        Left a -> undefined
        Right r -> return r
    
tokenizer :: String -> IO [String]
tokenizer line = tokenizer' (return [""]) line

tokenizer' :: IO [String] -> String -> IO [String]
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

read_form :: [String] -> (MalType, [String])
read_form tokens = do
    case (head tokens) of
        '(' -> read_list (tail tokens)
        otherwise -> (read_atom (head tokens), (tail tokens))

read_list :: [String] -> (MalList [MalType], [String])
read_list (x:xs) =
    case x of
        ')' -> (MalList [], xs)
        otherwise -> (MalList (read_form(x:xs):read_list xs), xs)
    
read_atom :: String -> MalType
read_atom = undefined

