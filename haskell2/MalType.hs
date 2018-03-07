module MalType (MalType(MalString, MalNumber, MalKeyword, MalList, MalVector, MalSymbol, MalNil, MalTrue, MalFalse)) where

data MalType = MalString String | MalNumber Int | MalList [MalType] | MalVector [MalType] | MalSymbol String | MalKeyword String | MalNil | MalTrue | MalFalse deriving (Show, Eq)
