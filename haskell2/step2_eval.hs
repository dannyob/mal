{-# LANGUAGE OverloadedStrings #-}

import System.IO
import Reader
import Printer
import MalType
-- import Data.Map

malREAD :: [String] -> Either String MalType
malREAD x = do
        rf <- read_form x
        return (fst $ rf)

malEVAL :: [(String, MalType)] -> MalType -> MalType
malEVAL env (MalList []) = MalList []
-- malEVAL env (MalList x) = let result = eval_ast env (MalList x) in case result of
--    MalList ((MalBuiltinFunction f):ys) -> (f ys)
malEVAL env (MalList x) = case eval_ast env (MalList x) of
    MalList ((MalBuiltinFunction f):ys) -> (f ys)
    -- TODO: Handle error case when we attempt to call a non-function by
    --       evaluating a list with a non-function as its first element.
malEVAL env x = eval_ast env x

plus :: [MalType] -> MalType
plus ((MalNumber m):(MalNumber n):_) = MalNumber (m + n)

minus :: [MalType] -> MalType
minus ((MalNumber m):(MalNumber n):_) = MalNumber (m - n)

times :: [MalType] -> MalType
times ((MalNumber m):(MalNumber n):_) = MalNumber (m * n)

divided :: [MalType] -> MalType
divided ((MalNumber m):(MalNumber n):_) = MalNumber (m `div` n)

equals :: [MalType] -> MalType
equals (m:n:_) = if (m==n) then MalTrue else MalFalse

unknownfunction :: [MalType] -> MalType
unknownfunction _ = MalNil

count :: [MalType] -> MalType
count ((MalList l):_) = MalNumber (length l)
count ((MalString s):_) = MalNumber (length s)
count ((MalVector v):_) = MalNumber (length v)

repl_env = [("+", MalBuiltinFunction plus),
            ("-", MalBuiltinFunction minus),
            ("*", MalBuiltinFunction times),
            ("/", MalBuiltinFunction divided),
            ("=", MalBuiltinFunction equals),
            ("count", MalBuiltinFunction count)]

unwrap :: Maybe MalType -> MalType
unwrap (Just x) = x
unwrap Nothing = MalBuiltinFunction unknownfunction
-- TODO: Currently attempting to look up an unbound symbol will return
--       the unknownfunction function, which is \a -> MalNil. Make this
--       instead cause an error that interrupts interpretation but doesn't
--       crash the interpreter.

eval_ast :: [(String, MalType)] -> MalType -> MalType
eval_ast env (MalSymbol x) = unwrap (lookup x env)
eval_ast env (MalList x) = MalList (map (malEVAL env) x)
eval_ast env x = x

malPRINT :: MalType -> String
malPRINT x = pr_str x True

rep :: [String] -> String
rep x = case malREAD x of
        Left err -> err
        Right mts -> malPRINT $ malEVAL repl_env mts

repl = do
    putStr "user> "
    hFlush stdout
    input <- getLine
    tokens <- tokenizer input
    if tokens /= [] then do
        putStrLn $ rep tokens
        hFlush stdout
    else return ()
    repl

main = repl
