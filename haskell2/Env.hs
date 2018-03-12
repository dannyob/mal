module Env () where
import MalType
import Data.Map
import Data.Maybe


data MalEnvEntry = MalEnvEntry { meId :: Int , meOuter :: Maybe Int , meValues :: Map
String MalType } deriving Show

data MalEnvBag = MalEnvBag { mebNextId :: Int , mebBag :: (Map Int MalEnvEntry)  } deriving Show

data MalEnv = MalEnv MalEnvBag Int deriving Show

set :: MalEnv -> MalType -> MalType -> MalEnv

-- set (MalEnv bag index) (MalSymbol s) v = 
--    let newBag i = MalEnvBag (mebNextId bag) i
--        thisEnv = fromMaybe Data.Map.empty $ meValues (Data.Map.findWithDefault testEnvEntry index (mebBag bag)) in
--    MalEnv (newBag (Data.Map.insert index bag $ Data.Map.insert s v thisEnv)) index
--
set (MalEnv bag index) (MalSymbol s) v = 
    let newEnvBag b = MalEnvBag (mebNextId bag) b
        newEnvEntry old = MalEnvEntry (meId old) (meOuter old) (Data.Map.insert s v (meValues old))
        oldBag = mebBag bag
        newBag = Data.Map.insert index (newEnvEntry $ Data.Map.findWithDefault testEnvEntry index oldBag) oldBag
    in MalEnv (newEnvBag newBag) index

set _ _ _ = undefined

find :: MalEnv -> MalType -> MalEnv

find = undefined

get :: MalEnv -> MalType -> Maybe MalEnv
get (MalEnv bag index) (MalSymbol s) 

get = undefined

testRootEnv = Data.Map.fromList [("seventeen", MalNumber 17)]
testEnvEntry = MalEnvEntry 0 Nothing testRootEnv
bag = MalEnvBag 1 (Data.Map.fromList [(0, testEnvEntry)])




