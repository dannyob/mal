module MalType (MalType(MalString, MalNumber, MalKeyword, MalList, MalVector, MalSymbol, MalNil, MalTrue, MalFalse, MalBuiltinFunction)) where

data MalType = MalString String | MalNumber Int | MalList [MalType] | MalVector [MalType] | MalSymbol String | MalKeyword String | MalNil | MalTrue | MalFalse | MalBuiltinFunction ([MalType] -> MalType)

instance Show MalType where
    show (MalString s) = "<MalString " ++ s ++ ">"
    show (MalNumber n) = "<MalNumber " ++ show n ++ ">"
    show (MalList l) = "<MalList " ++ show l ++ ">"
    show (MalVector v) = "<MalVector " ++ show v ++ ">"
    show (MalSymbol s) = "<MalSymbol " ++ s ++ ">"
    show (MalKeyword s) = "<MalKeyword " ++ s ++ ">"
    show MalNil = "<MalNil>"
    show MalTrue = "<MalTrue>"
    show MalFalse = "<MalFalse>"
    show (MalBuiltinFunction f) = "<MalBuiltinFunction>"

instance Eq MalType where
    MalString s == MalString t = s == t
    MalString s == _ = False
    MalNumber s == MalNumber t = s == t
    MalNumber s == _ = False
    MalList s == MalList t = s == t
    MalList s == _ = False
    MalVector s == MalVector t = s == t
    MalVector s == _ = False
    MalSymbol s == MalSymbol t = s == t
    MalSymbol s == _ = False
    MalKeyword s == MalKeyword t = s == t
    MalKeyword s == _ = False
    MalNil == MalNil = True
    MalNil == _ = False
    MalTrue == MalTrue = True
    MalTrue == _ = False
    MalFalse == MalFalse = True
    MalFalse == _ = False
    MalBuiltinFunction f == _ = False
    -- TODO: Add function ID so that a builtin function can equal itself
