module MalType (MalType(MalString, MalNumber, MalList, MalSymbol, MalNil, MalTrue, MalFalse)) where

data MalType = MalString String | MalNumber Int | MalList [MalType] | MalSymbol String | MalNil | MalTrue | MalFalse deriving Show


