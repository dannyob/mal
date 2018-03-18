module Env () where
import MalType
import Data.Map
import Data.Maybe
import Data.IntMap as DI


data MalEnvEntry = MalEnvEntry { meOuter :: Maybe Int , meData :: Map String MalType } deriving Show
data MalEnv = MalEnv { meBag :: DI.IntMap MalEnvEntry, meCurrent :: Int } deriving Show

set :: MalEnv -> MalType -> MalType -> MalEnv
set (MalEnv oldBag current) (MalSymbol k) v =
    let updateEntry (MalEnvEntry outer dat) = MalEnvEntry outer (Data.Map.insert k v dat)
        updateBag = DI.adjust updateEntry current oldBag in
        MalEnv updateBag current

set _ _ _ = undefined

find :: MalEnv -> MalType -> MalEnv
find = undefined

get :: MalEnv -> MalType -> Maybe MalEnv
get = undefined

emptyRootEnv = Data.Map.empty
emptyEnvEntry = MalEnvEntry Nothing emptyRootEnv
env = MalEnv (DI.fromList [(0, emptyEnvEntry)]) 0

testRootEnv = Data.Map.fromList [("seventeen", MalNumber 17)]
testChildEnv = Data.Map.fromList [("sixteen", MalNumber 16)]

testEnv = MalEnv (DI.fromList [(0, MalEnvEntry Nothing testRootEnv), (1, MalEnvEntry (Just 0) testChildEnv)]) 1
