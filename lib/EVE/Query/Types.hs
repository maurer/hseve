module EVE.Query.Types where

-- | Parameters for EVE API calls. Formattable by eveQuery
data EVEParam = EInt Int
              | EStr String deriving (Ord, Eq)

-- | Sealable type representing a number we know is a corp ID
data CorpID = CoID {coidInner :: Int} deriving Show
-- | Sealable type representing a number we know is a char ID
data CharID = ChID {chidInner :: Int} deriving Show

data EVECred = EC { userID :: Int
                  , apiKey :: String
                  } deriving (Ord, Eq)
