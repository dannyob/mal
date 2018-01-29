module MalType (MalType(MalString, MalNumber, MalList, MalSymbol)) where

data MalType = MalString String | MalNumber Int | MalList [MalType] | MalSymbol String deriving Show


