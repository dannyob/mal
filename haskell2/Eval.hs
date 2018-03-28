module Eval (repl_env, malEVAL) where

import MalType
import Env

malEVAL :: MalEnv -> MalType -> (MalEnv, MalType)
malEVAL env (MalList ((MalSymbol "def!"):MalSymbol key:value:[])) = defpling env key value
malEVAL env (MalList ((MalSymbol "let*"):MalList bindings:result:[])) = letstar env bindings result
malEVAL env (MalList []) = (env, MalList [])
malEVAL env (MalList x) = case eval_ast env (MalList x) of
    (e, MalList ((MalBuiltinFunction f):ys)) ->  (e, (f ys))
    -- TODO: Handle error case when we attempt to call a non-function by
    --       evaluating a list with a non-function as its first element.

malEVAL env x = eval_ast env x

letstar :: MalEnv -> [MalType] -> MalType -> (MalEnv, MalType)
letstar env bindings result =
    let newenv = addChildMalEnv env []
        finalenv = multipledefs env bindings in
    (env, snd $ malEVAL finalenv result)

multipledefs :: MalEnv -> [MalType] -> MalEnv
multipledefs env bindings@(MalSymbol key:z:xs) =
    multipledefs (fst $ defpling env key z) xs
multipledefs env [] = env

defpling :: MalEnv -> String -> MalType -> (MalEnv, MalType)
defpling env k v = let r = snd (malEVAL env v) in
    (set env k r, r)

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

cons :: [MalType] -> MalType
cons (x:MalNil:_) = MalList [x]
cons (x:(MalList y):_) = MalList (x:y)

car :: [MalType] -> MalType
car (MalList (x:xs):_) = x

cdr :: [MalType] -> MalType
cdr (MalList (x:xs):_) = MalList xs

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
            ("count", MalBuiltinFunction count),
            ("cons", MalBuiltinFunction cons),
            ("car", MalBuiltinFunction car),
            ("cdr", MalBuiltinFunction cdr),
            ("seventeen", MalNumber 17)
           ]

unwrap :: Maybe MalType -> MalType
unwrap (Just x) = x
unwrap Nothing = MalBuiltinFunction unknownfunction
-- TODO: Currently attempting to look up an unbound symbol will return
--       the unknownfunction function, which is \a -> MalNil. Make this
--       instead cause an error that interrupts interpretation but doesn't
--       crash the interpreter.

eval_ast :: MalEnv -> MalType -> (MalEnv, MalType)
eval_ast env (MalSymbol x) = (env, unwrap (get env x))
-- The following may have to become folds or somesuch
-- in order to collect changes in the environment:
eval_ast env (MalList x) = (env, MalList (map (snd . malEVAL env) x))
eval_ast env (MalVector x) = (env, MalVector (map (snd . malEVAL env) x))
eval_ast env x = (env, x)
