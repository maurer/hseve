-- | A module holding various types used by multiple submodules of
--   "EVE.Query"
module EVE.Query.Types where

-- | Parameters for EVE API calls. Formattable by
--   'EVE.Query.XML.eveQuery'
data EVEParam = EInt Int
              | EStr String deriving (Ord, Eq)

-- | Sealable type representing a number we know is a corp ID
data CorpID = CoID {coidInner :: Int} deriving Show
-- | Sealable type representing a number we know is a char ID
data CharID = ChID {chidInner :: Int} deriving Show

-- | EVE Credentials. Only valid for new-style credentials,
--   we don't support the old style as they are deprecated
--   anyways. Note that 'userID' does not actually pertain
--   to a user, but instead to a key. This is just the terminology
--   used by the API docs and interface. It's kind of weird.
data EVECred = EC { userID :: Int
                  , apiKey :: String
                  } deriving (Ord, Eq)
