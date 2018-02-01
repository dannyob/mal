module Reader (tokenizer, read_form) where

import Data.Char
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
tokenizer line = tokenizer' (return []) line

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
    let (mallist, malrest) = read_list (tail tokens)
    let (malvec, malrest') = read_vector (tail tokens)
    case (head tokens) of
        "(" -> (MalList mallist, malrest)
        "[" -> (MalVector malvec, malrest')
        otherwise -> (read_atom (head tokens), (tail tokens))

read_list :: [String] -> ([MalType], [String])
read_list [] = undefined
read_list (x:xs) =
    let (first_list, rest_list) = read_list xs  in
    let (first_form, rest_form) = read_form (x:xs) in
    case x of
        ")" -> ([], xs)
        otherwise -> (first_form : (fst $ read_list rest_form), rest_list)

read_vector :: [String] -> ([MalType], [String])
read_vector [] = undefined
read_vector (x:xs) =
    let (first_list, rest_list) = read_vector xs  in
    let (first_form, rest_form) = read_form (x:xs) in
    case x of
        "]" -> ([], xs)
        otherwise -> (first_form : (fst $ read_vector rest_form), rest_list)

-- This will crash with no base case if you somehow get it to attempt to
-- read a string that doesn't end with a quotation mark.
read_string False "\"" = ""
read_string escaped (x:xs)
    -- (These cases could be consolidated carefully with boolean logic.)
    | escaped && (x == 'n') = '\n':(read_string False xs)
    | escaped = x:(read_string False xs)
    | x == '\\' = read_string True xs
    | otherwise = x:(read_string False xs)

read_string' (x:xs)
    | x == '"' = read_string False xs

read_atom :: String -> MalType
read_atom s
    | s == "nil" = MalNil
    | s == "true" = MalTrue
    | s == "false" = MalFalse
    | head s == '"' = MalString (read_string' s)
    | Data.Char.isAlpha(head s) = MalSymbol s
    | head s == '-' && length s > 1 && Data.Char.isDigit(head $ tail s) = MalNumber (read s)
    | Data.Char.isDigit(head s) = MalNumber (read s)
    | elem (head s) "+-*/!%" = MalSymbol s
    -- TODO: MalSymbol should possibly also cover some other leading punctuation
