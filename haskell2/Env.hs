module Env (MalEnv, mkMalEnv, get, find, set) where
import MalType
import Data.Map
import Data.Maybe
import Data.IntMap as DI


data MalEnvEntry = MalEnvEntry { meOuter :: Maybe Int , meData :: Map String MalType } deriving Show
data MalEnv = MalEnv { meBag :: DI.IntMap MalEnvEntry, meCurrent :: Int } deriving Show

set :: MalEnv -> String -> MalType -> MalEnv
set (MalEnv oldBag current) k v =
    let updateEntry (MalEnvEntry outer dat) = MalEnvEntry outer (Data.Map.insert k v dat)
        updateBag = DI.adjust updateEntry current oldBag in
        MalEnv updateBag current

find :: MalEnv -> String -> Maybe MalEnv
find me@(MalEnv bag current) k =
    if Data.Map.member k (meData $ bag DI.! current) then 
    Just me
    else
        do
            parent <- meOuter $ bag DI.! current
            find (MalEnv bag parent) k

get :: MalEnv -> String -> Maybe MalType
get me@(MalEnv bag current) k  = do
    f <- find me k
    Data.Map.lookup k (getCurrentData f)

mkMalEnv :: [(String,MalType)] -> MalEnv
mkMalEnv xs = MalEnv (DI.fromList [(0, MalEnvEntry Nothing (Data.Map.fromList xs))]) 0

getCurrentData :: MalEnv -> Map String MalType
getCurrentData me@(MalEnv bag current) = meData (bag DI.! current)
