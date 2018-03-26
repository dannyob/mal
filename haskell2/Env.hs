module Env (MalEnv, mkMalEnv) where
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

find :: MalEnv -> MalType -> Maybe MalEnv
find me@(MalEnv bag current) (MalSymbol k) =
    if Data.Map.member k (meData $ bag DI.! current) then 
    Just me
    else
        do
            parent <- meOuter $ bag DI.! current
            find (MalEnv bag parent) (MalSymbol k)

get :: MalEnv -> MalType -> Maybe MalType
get me@(MalEnv bag current) (MalSymbol k)  = do
    f <- find me (MalSymbol k)
    Data.Map.lookup k (getCurrentData f)

mkMalEnv :: [(String,MalType)] -> MalEnv
mkMalEnv xs = MalEnv (DI.fromList [(0, MalEnvEntry Nothing (Data.Map.fromList xs))]) 0

getCurrentData :: MalEnv -> Map String MalType
getCurrentData me@(MalEnv bag current) = meData (bag DI.! current)
