module EVE.Monad where

import Control.Monad.Error
import Control.Monad.Reader
import Data.Typeable

data EVECred = EC { userID :: Int
                  , apiKey :: String
      }

data EVEParam = EInt Int
              | EStr String

data CorpID = CoID {coidInner :: Int} deriving Show
data CharID = ChID {chidInner :: Int} deriving Show

data EVEError = StructError
              | AuthError
              | APIErrorCode Int
              | ValueParseError
              | APIVersionError
              | OtherError String deriving (Show, Typeable)

instance Error EVEError where
  strMsg = OtherError

type EVE = ErrorT EVEError (ReaderT EVECred IO)

runEVE :: EVECred -> EVE a -> IO (Either EVEError a)
runEVE c m = runReaderT (runErrorT m) c

getCreds :: EVE EVECred
getCreds = ask
