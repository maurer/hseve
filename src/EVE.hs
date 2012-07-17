module EVE (
EVECred(..),
runEVE,
getCharacters,
CorpID,
CharID,
coidInner,
chidInner
) where

import Data.String
import Data.Maybe
import Control.Monad.Reader
import Data.Conduit
import Network.HTTP.Conduit
import Text.XML
import Data.Map ((!))
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.Char8 as LBS

data EVECred = EC { userID :: Int
                  , apiKey :: String
		  }

data EVEParam = EInt Int
              | EStr String

data CorpID = CoID {coidInner :: Int} deriving Show
data CharID = ChID {chidInner :: Int} deriving Show

type EVE = ReaderT EVECred IO

runEVE :: EVECred -> EVE a -> IO a
runEVE = flip runReaderT

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
  withManager $ \m -> do xml <- fmap responseBody $ http req' m
                         xml $$ sinkDoc def
  where format (s, v) = let
          v' = case v of
                 EInt n -> show n
                 EStr n -> n
          in (BS.pack s, BS.pack v')

isElement (NodeElement _) = True
isElement _ = False

toElement (NodeElement x) = x

(!?) :: Element -> String -> Element
e !? n = head $ filter ((== (show n)) . show . nameLocalName . elementName)
           (map toElement $ filter isElement $ elementNodes e)
(!*) :: Element -> String -> String
e !* n = show $ fromJust $ lookup (Name (fromString n) Nothing Nothing) (elementAttributes e)

extractRowset f doc = let
  root = documentRoot doc
  rows = map toElement $ filter isElement $ elementNodes $ root !? "result" !? "rowset"
  in map f rows

--The EVE monad should carry an implicit error other than IO's. This should be fixed and validation added, but I would like to at least see this much work first.
getCharacters :: EVE [( String -- ^ Character name
                      , CharID -- ^ Character ID
                      , String -- ^ Corp name
                      , CorpID -- ^ Corp ID
                      )
                     ]
getCharacters = fmap (extractRowset charExtract) $ eveQuery "account" "characters" []
   where charExtract row = (read $ row !* "name",
                            ChID $ read $ read $ row !* "characterID",
                            read $ row !* "corporationName",
                            CoID $ read $ read $ row !* "corporationID")
