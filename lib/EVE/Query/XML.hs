-- | This module handles sending and receiving raw queries
--   from the EVE API. It handles the HTTP request, auth,
--   error checking, and the XML-interaction utility functions
--   used by 'EVE.Query' to implement each call.
module EVE.Query.XML  where

import Data.Conduit
import Network.HTTP.Conduit
import Text.XML
import Control.Monad.Error
import EVE.Monad
import EVE.Query.Types
import qualified Data.Text as T
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.Char8 as LBS

import Data.Time

-- | This is the URL we are querying. No need to parameterize
--   this unless we want to do testing later, as there's only
--   one instance of EVE.
baseURL :: String
baseURL = "https://api.eveonline.com/"

-- | This is the meat of the module. It takes in a description
--   of a query the user wants performed, then fetches it,
--   checking errors along the way.
eveQuery :: EVECred        -- ^ Credentials
         -> String         -- ^ Category
         -> String         -- ^ Operation
	 -> [(String, EVEParam)] -- ^ Parameter map (argname, val)
	 -> EVE Element          -- ^ Resultant XML
eveQuery creds cat op params = do
  let q = (creds, (cat, op, params))
  mr <- eveCacheCheck q
  case mr of
    Just r  -> return r
    Nothing -> do
      let params' = ("keyID", EInt $ userID creds):
                    ("vCode", EStr $ apiKey creds):
                    params
      let url = baseURL ++ cat ++ "/" ++ op ++ ".xml.aspx"
      req <- parseUrl url
      let req' = urlEncodedBody (map format params') req
      doc <- withManager $ \m -> do xml <- fmap responseBody $ http req' m
                                    xml $$ sinkDoc def
      elm  <- apiCheck doc
      elm' <- checkError elm
      t0   <- readContents elm "currentTime"
      t1   <- readContents elm "cachedUntil"
      let dt = diffUTCTime t1 t0
      eveCacheReg q elm' dt
      return elm'
  where format (s, v) = let
          v' = case v of
                 EInt n -> show n
                 EStr n -> n
          in (BS.pack s, BS.pack v')

-- | Parses the non-XML contents of a selected subnode of a given
--   node. This may seem odd to fuse '(!?)' and this functionality,
--   but in practice these two have occurred together every time.
readContents :: (Read a)
             => Element -- ^ Parent
             -> String  -- ^ Child branch
             -> EVE a
readContents elm p = readE =<< (fmap nodeContents $ (return elm) !? p)
  where nodeContents = (concatMap exContent) . elementNodes
        exContent (NodeContent x) = T.unpack x
        exContent _ = ""

-- | Helper predicate for what kind of a 'Node' somthing is
isElement (NodeElement _) = True
isElement _ = False

-- | Partial function valid only when 'isElement' returns true
--   for unwrapping a 'Node'
toElement (NodeElement x) = x

-- | 'Element' indexing on an 'Element'. To allow chaining,
--   it is written in a style where it takes a monadic action
--   and produces a new one. It cannot be non-monadic,
--   as the arm may not exist, and it needs to throw an error
--   into the monad in that case.
(!?) :: EVE Element -- ^ Chainable element
     -> String      -- ^ Branch choice
     -> EVE Element
e' !? n = do
  e <- e'
  case filter ((== (show n)) . show . nameLocalName . elementName)
               (map toElement $ filter isElement $ elementNodes e) of
    x:_ -> return x
    _ -> throwError StructError

-- | Attribute indexing on an 'Element'. Otherwise the same as
--   '(!?)'
(!*) :: EVE Element -- ^ Chainable element
     -> String      -- ^ Branch choice
     -> EVE String
e' !* n = do
  e <- e'
  case lookup (name n) (elementAttributes e) of
    Just x -> return $ T.unpack x
    _ -> throwError StructError

-- | 'read', but set up so that it throws the error into the 'EVE'
--   monad instead of using 'error', allowing for better error
--   handling.
readE :: (Read a)
      => String -- ^ String to be parsed
      -> EVE a
readE s =
  case reads s of
    [(x,"")] -> return x
    _ -> throwError ValueParseError

-- | Generates an XML version of a string as a name, assuming
--   no namespacing is going on.
name s = Name (T.pack s) Nothing Nothing

-- | Checks that we have an xml document that respresents our
--   expected API version response, returns the component of
--   the doc that matters.
apiCheck :: Document -> EVE Element
apiCheck doc = do
  let root = documentRoot doc
  assertError StructError $ (elementName root) == (name "eveapi")
  assertError APIVersionError $
    case lookup (name "version") (elementAttributes root) of
      Just v  -> (T.unpack v) == "2"
      Nothing -> False
  return $ root

-- | Converts EVE API error codes to 'EVEError's.
--   Anything that goes to 'APIErrorCode' in practice is a bug.
errorCode :: Int -> EVEError
errorCode 203 = AuthError
errorCode n = APIErrorCode n

-- | Checks for an error according to the API's error reporting.
checkError :: Element -> EVE Element
checkError doc = do
  e <- catchError (fmap Just $ readE =<< (return doc) !? "error" !* "code")
                  (\_ -> return Nothing)
  case e of
    Just n  -> throwError $ errorCode n
    Nothing -> return ()
  (return doc) !? "result"

-- | A convenience function for interacting with the "rowset"
--   element used in some responses. Takes a projector on rows
--   and applies it to each row in turn. Does monadic chaining
--   a la '(!?)'.
mapRowset :: (EVE Element -> EVE a) -- ^ Projection of a row
          -> EVE Element        -- ^ Action to get the element
          -> EVE [a]
mapRowset f elm = do
  rows <- fmap (map toElement . filter isElement . elementNodes) $ elm !? "rowset"
  mapM (f . return) rows
