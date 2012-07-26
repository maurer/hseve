-- | This module contains the implementations of all the different
--   individual queries. This may be broken down by category later.
module EVE.Query where

import EVE.Monad
import EVE.Query.XML
import EVE.Query.Types
import Data.Time

-- | Sealable type representing a number we know is a corp ID
data CorpID = CoID {coidInner :: Int} deriving Show
-- | Sealable type representing a number we know is a char ID
data CharID = ChID {chidInner :: Int} deriving Show
-- | Sealable type representing a linked account ID/key pair.
data Account = AcID { accIdInner  :: Int
                    , accKeyInner :: Int
                    } deriving Show
-- | Type representing ISK with appropriate read/show
data ISK = ISK {iskInner :: Int}
instance Show ISK where
  show (ISK v) = (show (v `div` 100)) ++ "." ++ (show (v `mod` 100))
instance Read ISK where
  readsPrec x v = let
    (k, s) = unzip $ readsPrec x v
    in zip (map (\z -> ISK $ floor $ z * 100) k) s

-- | Represents the handles to a character's information
data CharIdent = CI { charName :: String -- ^ Character name
                    , charID   :: CharID -- ^ Character ID
                    , corpName :: String -- ^ Corp Name
                    , corpID   :: CorpID -- ^ Corp ID
                    }

-- | Takes in credentials, provides a list of character info they
--   are willing to accept these for.
getCharacters :: EVECred         -- ^ Credentials
              -> EVE [CharIdent] -- ^ Character ID records
getCharacters ec =
  mapRowset charExtract $ eveQuery ec "account" "characters" []
  where charExtract row = do
          name <- row !* "name"
          chid <- fmap ChID $ readE =<< row !* "characterID"
          corp <- row !* "corporationName"
          coid <- fmap CoID $ readE =<< row !* "corporationID"
          return $ CI name chid corp coid

-- | Contains information about billing and playtime.
data AccountStatus =
  AcStat { paidUntil    :: UTCTime -- ^ Expiry time
         , createDate   :: UTCTime -- ^ Account creation time
         , logonCount   :: Int -- ^ Number of times logged in
         , logonMinutes :: Int -- ^ Amount of time in-game
         } deriving Show

-- | For user keys, provides information about billing and playtime
getAccStatus :: EVECred -- ^ User credentials
             -> EVE AccountStatus
getAccStatus ec = do
  r <- eveQuery ec "account" "AccountStatus" []
  pu <- readContents r "paidUntil"
  cd <- readContents r "createDate"
  lc <- readContents r "logonCount"
  lm <- readContents r "logonMinutes"
  return $ AcStat { paidUntil    = pu
                  , createDate   = cd
                  , logonCount   = lc
                  , logonMinutes = lm
                  }

getBalances :: EVECred -- ^ User credentials
            -> CharID  -- ^ Which character to query
            -> EVE [(Account, ISK)]
getBalances ec chid = do
  mapRowset balExtract $ eveQuery ec "char" "AccountBalance"
                                  [("characterID", EInt $ chidInner chid)]
  where balExtract row = do
          accID  <- readE =<< row !* "accountID"
          accKey <- readE =<< row !* "accountKey"
          val    <- readE =<< row !* "balance"
          return (AcID accID accKey, val)
