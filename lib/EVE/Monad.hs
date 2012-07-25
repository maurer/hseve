-- | Contains the glue to declare the EVE monad and then
--   embed the various queries and error handling into it.
module EVE.Monad where

import Control.Monad.Error
import Control.Monad.Reader
import Control.Monad.State
import Text.XML
import EVE.Query.TimedCache
import EVE.Query.Types
import Data.Typeable
import Data.Time

-- | All the kinds of errors that can happen. If "OtherError"
--   is ever encountered in practice, it should be considered
--   a bug, but it is useful to have for debugging and to allow
--   other things to throw errors for the moment.
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
-- | Our queries are just the tupled form of what is given to
--   the HTTP/XML query transport.
type Query = (EVECred, (String, String, [(String, EVEParam)]))
-- | Our responses are just XML elements
type Response = Element
-- | 'Cache' is the kind of 'TimedCache' that we are using.
type Cache = TimedCache Query Response

-- | The monad itself. It has 'ErrorT' for explicit error handling,
--   'StateT' for API response caching, and 'IO' to actually run
--   requests.
type EVE = ErrorT EVEError (StateT Cache IO)

-- | Runs an action in the EVE monad. Note that this currently
--   uses a new cache manager per-invocation, so you should not
--   call this frequently, instead preferring to embed all your
--   calls into a single monadic action.
runEVE :: EVE a -> IO (Either EVEError a)
runEVE m = evalStateT (runErrorT m) empty

-- | 'assert' with a specified error to throw within the 'EVE'
--   monad. This really should be in "Control.Monad.Error" in a
--   generic form, but for some reason doesn't seem to be.
assertError :: EVEError -> Bool -> EVE ()
assertError e b = if b then return () else throwError e

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
