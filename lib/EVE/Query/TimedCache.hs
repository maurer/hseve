-- | This module provides a simple timed expiry cache, designed to allow
--   us to rate limit queries to the EVE API while still allowing client
--   programs to query however often they'd like.
module EVE.Query.TimedCache where

import qualified Data.Map as Map
import Data.Map (Map)
import Data.Time

-- | The type representing cache.
--   It is functional, so you will have to either embed it in State/StateT
--   or some kind of IORef/MVar/TVar
type TimedCache query response = Map query (response, UTCTime)

-- If this ever gets slow, we could change it to be a map on expiry time
-- onto a map from queries, and see if there is a way to use the structure
-- of the map to dump more efficiently

-- | An empty cache
empty = Map.empty

-- | Register an answer with the cache
cacheReg :: (Ord q)
         => q              -- ^ Query
         -> r              -- ^ Response
         -> UTCTime        -- ^ Time to expire at
         -> TimedCache q r -- ^ Old cache
         -> TimedCache q r -- ^ New cache
cacheReg k v t c = Map.insert k (v, t) c

-- Possibly, the cache purging in cache check should be separated
-- and added to cacheReg.
-- However, since I would expect cacheCheck to be run immediately
-- before cacheReg in most cases, it would usually be redundant

-- | See if the query is in the cache; purge dead entries
cacheCheck :: (Ord q)
           => q              -- ^ Query
           -> UTCTime        -- ^ Time for purging
           -> TimedCache q r -- ^ Cache
           -> (Maybe r, TimedCache q r)
cacheCheck k t m = let
  m' = Map.filter (\(_,t') -> t < t') m
  in (fmap fst $ Map.lookup k m', m')
