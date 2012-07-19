module EVE.Query.TimedCache where

import qualified Data.Map as Map
import Data.Map (Map)
import Data.Time

type TimedCache query response = Map query (response, UTCTime)

empty = Map.empty

cacheReg :: (Ord q) => q -> r -> UTCTime -> TimedCache q r -> TimedCache q r
cacheReg k v t c = Map.insert k (v, t) c

cacheCheck :: (Ord q) => q -> UTCTime -> TimedCache q r -> (Maybe r, TimedCache q r)
cacheCheck k t m = let
  m' = Map.filter (\(_,t') -> t < t') m
  in (fmap fst $ Map.lookup k m', m')

cacheMerge :: (Ord q) => [TimedCache q r] -> TimedCache q r
cacheMerge = Map.unions
