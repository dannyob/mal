module MalType (MalType) where

data MalType = MalString String | MalNumber Int | MalList [MalType] | MalSymbol String deriving Show


