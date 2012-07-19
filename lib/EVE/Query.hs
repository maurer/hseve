module EVE.Query where

import EVE.Monad
import EVE.Query.XML
import EVE.Query.Types
import Data.Time

data CharIdent = CI { charName :: String -- ^ Character name
                    , charID   :: CharID -- ^ Character ID
                    , corpName :: String -- ^ Corp Name
                    , corpID   :: CorpID -- ^ Corp ID
                    }

getCharacters :: EVECred  -- ^ Credentials
              -> EVE [CharIdent] -- ^ Character ID records
getCharacters ec = extractRowset charExtract $ eveQuery ec "account" "characters" []
   where charExtract row = do
           name <- row !* "name"
           chid <- fmap ChID $ readE =<< row !* "characterID"
           corp <- row !* "corporationName"
           coid <- fmap CoID $ readE =<< row !* "corporationID"
           return $ CI name chid corp coid

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
