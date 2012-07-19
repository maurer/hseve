module EVE.Monad where

import Control.Monad.Error
import Control.Monad.Reader
import Control.Monad.State
import Text.XML
import EVE.Query.TimedCache
import EVE.Query.Types
import Data.Typeable
import Data.Time

data EVEError = StructError
              | AuthError
              | APIErrorCode Int
              | ValueParseError
              | APIVersionError
              | OtherError String deriving (Show, Typeable)

instance Error EVEError where
  strMsg = OtherError
--TODO For memory performance, I should try to move the cache out of
--the xml layer. This is easier for now though.
type Query = (EVECred, (String, String, [(String, EVEParam)]))
type Response = Element
type Cache = TimedCache Query Response

type EVE = ErrorT EVEError (StateT Cache IO)

-- | Runs an action in the EVE monad. Note that this currently
--   uses a new cache manager per-invocation, so you should not
--   call this frequently, instead preferring to embed all your
--   calls into a single monadic action.
runEVE :: EVE a -> IO (Either EVEError a)
runEVE m = evalStateT (runErrorT m) empty

-- | Logs a query / response pair into the cache
eveCacheReg :: Query           -- ^ The question
            -> Response        -- ^ The answer
            -> NominalDiffTime -- ^ Time delta to expiry
            -> EVE ()
eveCacheReg q r dt = do
  t0 <- liftIO $ getCurrentTime
  modify $ cacheReg q r $ addUTCTime dt t0

-- | Checks the cache for an answer to the question.
--   Dumps expired cache at the same time.
eveCacheCheck :: Query                -- ^ The question
              -> EVE (Maybe Response) -- ^ The response if present
eveCacheCheck q = do
  s <- get
  t <- liftIO $ getCurrentTime
  state $ cacheCheck q t
