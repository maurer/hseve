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

baseURL :: String
baseURL = "https://api.eveonline.com/"

eveQuery :: EVECred        -- ^ Credentials
         -> String         -- ^ Category
         -> String         -- ^ Operation
	 -> [(String, EVEParam)] -- ^ Parameters
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

readContents :: (Read a) => Element -> String -> EVE a
readContents elm p = readE =<< (fmap nodeContents $ (return elm) !? p)
  where nodeContents = (concatMap exContent) . elementNodes
        exContent (NodeContent x) = T.unpack x
        exContent _ = ""

isElement (NodeElement _) = True
isElement _ = False

toElement (NodeElement x) = x

(!?) :: EVE Element -> String -> EVE Element
e' !? n = do
  e <- e'
  case filter ((== (show n)) . show . nameLocalName . elementName)
               (map toElement $ filter isElement $ elementNodes e) of
    x:_ -> return x
    _ -> throwError StructError

(!*) :: EVE Element -> String -> EVE String
e' !* n = do
  e <- e'
  case lookup (name n) (elementAttributes e) of
    Just x -> return $ T.unpack x
    _ -> throwError StructError

readE :: (Read a) => String -> EVE a
readE s =
  case reads s of
    [(x,"")] -> return x
    _ -> throwError ValueParseError

name s = Name (T.pack s) Nothing Nothing

assertError :: EVEError -> Bool -> EVE ()
assertError e b = if b then return () else throwError e

apiCheck :: Document -> EVE Element
apiCheck doc = do
  let root = documentRoot doc
  assertError StructError $ (elementName root) == (name "eveapi")
  assertError APIVersionError $
    case lookup (name "version") (elementAttributes root) of
      Just v  -> (T.unpack v) == "2"
      Nothing -> False
  return $ root

errorCode :: Int -> EVEError
errorCode 203 = AuthError
errorCode n = APIErrorCode n

checkError :: Element -> EVE Element
checkError doc = do
  e <- catchError (fmap Just $ readE =<< (return doc) !? "error" !* "code")
                  (\_ -> return Nothing)
  case e of
    Just n  -> throwError $ errorCode n
    Nothing -> return ()
  (return doc) !? "result"

extractRowset f elm = do
  rows <- fmap (map toElement . filter isElement . elementNodes) $ elm !? "rowset"
  mapM (f . return) rows
