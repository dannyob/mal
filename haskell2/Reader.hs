module Reader (tokenizer, read_form) where

import Data.Char
import Text.Regex.PCRE.String as TR
import MalType
import Debug.Trace

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
                    Right (Just (_, _ , after , [""])) | after == remaining -> return []
                    Right (Just (_, _ , after , [""])) -> tokenizer' (return (unwrapped_sofar)) after
                    Right (Just (_, _ , after , results)) -> tokenizer' (return (unwrapped_sofar ++ results)) after

read_form :: [String] -> Either String (MalType, [String])
read_form ("(":xs) = do
        (sequence, rest) <- (read_sequential xs ")")
        return (MalList sequence, rest)
read_form ("[":xs) = do
        (sequence, rest) <- (read_sequential xs "]")
        return (MalVector sequence, rest)
read_form (atom:xs) = do
        a <- read_atom atom
        return (a, xs)

read_sequential :: [String] -> String -> Either String ([MalType], [String])
read_sequential [] delim = Left $ "<Missing closing delimiter " ++ delim  ++ ">"
read_sequential (x:xs) delim = do
    if x == delim then Right $ ([], xs) else do
        (first_form, rest_form) <- read_form (x:xs)
        rs <- read_sequential rest_form delim
        return $ (first_form : fst rs, snd rs)

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

-- TODO: MalSymbol should possibly also cover some other leading punctuation
symbol_prefixes :: String
symbol_prefixes = "+-*/!%"

read_atom :: String -> Either String MalType
read_atom "nil" = Right MalNil
read_atom "true" = Right MalTrue
read_atom "false" = Right MalFalse
read_atom s@('"':xs) = Right $ MalString (read_string' s)
read_atom s
    | Data.Char.isAlpha(head s) = Right $ MalSymbol s
    | head s == '-' && length s > 1 && Data.Char.isDigit(head $ tail s) = Right $ MalNumber (read s)
    | Data.Char.isDigit(head s) = Right $ MalNumber (read s)
    | elem (head s) symbol_prefixes = Right $ MalSymbol s
    | otherwise = Left $ "<Cannot parse " ++ s ++ ">"

