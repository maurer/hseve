module EVE (
EVECred(..),
runEVE,
getCharacters,
CorpID,
CharID,
coidInner,
chidInner
) where

import Prelude hiding (catch)
import Data.String
import Data.Maybe
import Control.Monad.Reader
import Data.Conduit
import Network.HTTP.Conduit
import Text.XML
import Data.Map ((!))
import Control.Monad.Error
import Data.Typeable
import Control.Exception
import qualified Data.Text as T
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.Char8 as LBS

data EVECred = EC { userID :: Int
                  , apiKey :: String
		  }

data EVEParam = EInt Int
              | EStr String

data CorpID = CoID {coidInner :: Int} deriving Show
data CharID = ChID {chidInner :: Int} deriving Show

data EVEError = StructError
              | ValueParseError
              | OtherError String deriving (Show, Typeable)

instance Error EVEError where
  strMsg = OtherError

instance Exception EVEError

type EVE = ErrorT EVEError (ReaderT EVECred IO)

runEVE :: EVECred -> EVE a -> IO (Either EVEError a)
runEVE c m = runReaderT (runErrorT m) c

baseURL :: String
baseURL = "https://api.eveonline.com/"

eveQuery :: String               -- ^ Category
         -> String               -- ^ Operation
	 -> [(String, EVEParam)] -- ^ Parameters
	 -> EVE Document         -- ^ Resultant XML
eveQuery cat op params = do
  creds <- ask
  let params' = ("keyID", EInt $ userID creds):
                ("vCode", EStr $ apiKey creds):
		params
  let url = baseURL ++ cat ++ "/" ++ op ++ ".xml.aspx"
  req <- parseUrl url
  let req' = urlEncodedBody (map format params') req
  doc <- withManager $ \m -> do xml <- fmap responseBody $ http req' m
                                xml $$ sinkDoc def
  return doc
  where format (s, v) = let
          v' = case v of
                 EInt n -> show n
                 EStr n -> n
          in (BS.pack s, BS.pack v')

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
  case lookup (Name (fromString n) Nothing Nothing) (elementAttributes e) of
    Just x -> return $ T.unpack x
    _ -> throw StructError

readE :: (Read a) => String -> EVE a
readE s =
  case reads s of
    [(x,"")] -> return x
    _ -> throwError ValueParseError

extractRowset f doc = do
  let root = fmap documentRoot doc
  rows <- fmap (map toElement . filter isElement . elementNodes) $ root !? "result" !? "rowset"
  mapM (f . return) rows

--The EVE monad should carry an implicit error other than IO's. This should be fixed and validation added, but I would like to at least see this much work first.
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
