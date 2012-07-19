module EVE
(getCharacters
,module EVE.Monad
) where

import EVE.Query
import EVE.Monad

getCharacters :: EVE [( String -- ^ Character name
                      , CharID -- ^ Character ID
                      , String -- ^ Corp name
                      , CorpID -- ^ Corp ID
                      )
                     ]
getCharacters = extractRowset charExtract $ eveQuery "account" "characters" []
   where charExtract row = do
           name <- row !* "name"
           chid <- fmap ChID $ readE =<< row !* "characterID"
           corp <- row !* "corporationName"
           coid <- fmap CoID $ readE =<< row !* "corporationID"
           return (name, chid, corp, coid)

