module EVE.Query where

import EVE.Monad
import EVE.Query.XML
import Data.Time

getCharacters :: EVECred       -- ^ Credentials
              -> EVE [( String -- ^ Character name
                      , CharID -- ^ Character ID
                      , String -- ^ Corp name
                      , CorpID -- ^ Corp ID
                      )
                     ]
getCharacters ec = extractRowset charExtract $ eveQuery ec "account" "characters" []
   where charExtract row = do
           name <- row !* "name"
           chid <- fmap ChID $ readE =<< row !* "characterID"
           corp <- row !* "corporationName"
           coid <- fmap CoID $ readE =<< row !* "corporationID"
           return (name, chid, corp, coid)

data AccountStatus = AcStat { paidUntil    :: UTCTime
                            , createDate   :: UTCTime
                            , logonCount   :: Int
                            , logonMinutes :: Int
                            } deriving Show

getAccStatus :: EVECred -> EVE AccountStatus
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
