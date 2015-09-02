{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DoAndIfThenElse #-}
{-# LANGUAGE TupleSections #-}

import Control.Concurrent
import Control.Exception
import Control.Lens hiding ((+=), (.=), to)
import Control.Monad
import Data.String
import Data.Aeson
import Data.Aeson.Types
import Data.Csv ((.!))
import Data.Function hiding (id)
import Data.HashMap.Strict (HashMap)
import Data.List
import Data.Maybe
import Data.Monoid
import Data.Ord
import Data.Text (Text)
import Data.Text.Encoding
import GHC.Generics hiding (to)
import Network.Curl
import Network.Curl.Aeson
import Network.URI
import Network.Wreq hiding (Auth)
import qualified Network.HTTP.Client as HTTP.Client
import qualified Network.HTTP.Types as HTTP.Types
import Prelude hiding (id, log)
import Safe
import System.Console.CmdArgs hiding (name)
import System.Exit
import Text.HTML.TagSoup
import qualified Control.Logging as Logging
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Lazy.UTF8 as BL.UTF8
import qualified Data.Configurator as Configurator
import qualified Data.Csv as Csv
import qualified Data.HashMap.Strict as HashMap
import qualified Data.Text as Text
import qualified Data.Vector as Vector

-----------------------------------------------------------------------------
-- Command line interface
-----------------------------------------------------------------------------

data Crateman
  = Customers
  | Import {users :: FilePath}
  | Shipment {games :: FilePath}
  | Commit {shipment :: FilePath}
  | Refresh
  deriving (Show, Data)

interface :: Crateman
interface = modes [customers, imp, shipment, commit, refresh] &= summary "crateman v1.0"
  where
    customers = Customers &= help "Write customer list to customers.csv"
    imp = Import {users = def &= argPos 2 &= typFile} &= help "Import BGG user names from CSV file and update collections"
    shipment = Shipment {games = def &= argPos 0 &= typFile} &= help "Make shipment.csv, taking game inventory CSV file as input"
    commit = Commit {shipment = def &= argPos 1 &= typFile} &= help "Commit shipment information, taking a shipment CSV file as input"
    refresh = Refresh &= help "Refresh game collections from boardgamegeek.com"

-----------------------------------------------------------------------------
-- Cratejoy API helpers
-----------------------------------------------------------------------------

type Auth = (String, String)
type ItemId = Int
type CustomerId = ItemId

--waern :: CustomerId
--waern = 53504949

cratejoyApiUrl :: String
cratejoyApiUrl = "https://api.cratejoy.com/v1"

call :: (ToJSON a, FromJSON b) => Auth -> String -> String -> Maybe a -> IO b
call (user, pw) meth service = curlAeson parseJSON meth uri opts
  where
    uri = cratejoyApiUrl ++ service
    opts = [CurlUserPwd (user ++ ":" ++ pw), CurlSSLVersion 1 {- TLS -}]

parseResults :: FromJSON a => Value -> Parser a
parseResults (Object o) = o .: "results"
parseResults _ = mzero

getResults :: FromJSON a => Value -> IO a
getResults v =
  case parseEither parseResults v of
    Left msg -> exit ("Failed to get/parse results field: " <> Text.pack msg)
    Right x -> pure x

-----------------------------------------------------------------------------
-- Customers
-----------------------------------------------------------------------------

data Customer = Customer {id :: CustomerId, email :: Text, name :: Text}
  deriving (Eq, Show, Generic, FromJSON)

instance Csv.ToRecord Customer

getCustomers :: Auth -> IO [Customer]
getCustomers aut = do
  info "Fetching customers from Cratejoy..."
  r <- call aut "GET" "/customers/?limit=100000" noData
  customers <- getResults r
  info "Done."
  pure customers

-----------------------------------------------------------------------------
-- Subscriptions
-----------------------------------------------------------------------------

data Subscription = Subscription
  { customer :: Customer
  , status :: Text
  } deriving (Generic, Show, FromJSON)

isActive :: Text -> Bool
isActive = \case
  "active" -> True
  "renewing" -> True
  "unpaid" -> False
  "cancelled" -> False
  "suspended" -> False
  "expired" -> False
  "pastdue" -> False
  "pending_renewal" -> False
  _ -> False

getSubscriptions :: Auth -> IO [Subscription]
getSubscriptions aut = do
  info "Fetching subscriptions from Cratejoy..."
  r <- call aut "GET" "/subscriptions/?limit=100000" noData
  subscriptions <- getResults r
  info "Done."
  pure subscriptions

-----------------------------------------------------------------------------
-- Addresses
-----------------------------------------------------------------------------

data Address = Address
  { to :: Text
  , company :: Maybe Text
  , street :: Text
  , unit :: Maybe Text
  , city :: Text
  , zip_code :: Text
  , state :: Text
  , country :: Text
  } deriving (Generic, Show, FromJSON)

getCustomerAddresses :: Auth -> CustomerId -> IO [Address]
getCustomerAddresses aut cid = do
  r <- call aut "GET" ("/customers/" ++ show cid ++ "/addresses/") noData
  getResults r

getShippingAddress :: Auth -> Customer -> IO Address
getShippingAddress aut c = do
  l <- getCustomerAddresses aut (id c)
  case l of
    []  -> exit ("No addresses registered for customer: " <> Text.pack (show c))
    [a] -> pure a
    a : _ -> do warnCustomer c "more than one address for customer!"; pure a

-----------------------------------------------------------------------------
-- Preferences
-----------------------------------------------------------------------------

-- | Each pref is in the interval [0..4] 
data Prefs = Prefs
  { pPopular :: Int
  , pNewRelease :: Int
  , pForgotten :: Int
  , pFamily :: Int
  , pParty :: Int
  , pAbstract :: Int
  , pStrategy :: Int
  , p2Player :: Int
  , p3Player :: Int
  } deriving Show

-- TODO: change names in JSON/frontend?
instance FromJSON Prefs where
  parseJSON (Object o) = do
    pPopular <- o .: "popular"
    pNewRelease <- o .: "new_release"
    pForgotten <- o .: "forgotten"
    pFamily <- o .: "family"
    pParty <- o .: "party"
    pAbstract <- o .: "abstract"
    pStrategy <- o .: "strategy"
    p2Player <- o .: "player2"
    p3Player <- o .: "player3"
    pure Prefs
      { pPopular
      , pNewRelease
      , pForgotten
      , pFamily
      , pParty
      , pAbstract
      , pStrategy
      , p2Player
      , p3Player
      }
  parseJSON _ = mzero

instance ToJSON Prefs where
  toJSON (Prefs {..}) = object
    [ "popular" .= pPopular
    , "new_release" .= pNewRelease
    , "forgotten" .= pForgotten
    , "family" .= pFamily
    , "party" .= pParty
    , "abstract" .= pAbstract
    , "strategy" .= pStrategy
    , "player2" .= p2Player
    , "player3" .= p3Player
    ]

defaultPreferences :: Prefs
defaultPreferences = Prefs
  { pPopular = 2
  , pNewRelease = 2
  , pForgotten = 2
  , pFamily = 2
  , pParty = 2
  , pAbstract = 2
  , pStrategy = 2
  , p2Player = 2
  , p3Player = 2
  }

-----------------------------------------------------------------------------
-- Games
-----------------------------------------------------------------------------

data Game = Game {gameId :: ItemId, gameTitle :: Text} deriving Show

instance Eq Game where
  (==) = (==) `on` gameId

instance FromJSON Game where
  parseJSON (Object o) = do
    gameId <- o .: "id"
    gameTitle <- o .: "title"
    pure Game {gameId, gameTitle}
  parseJSON _ = mzero

instance ToJSON Game where
  toJSON (Game id title) = object ["id" .= id, "title" .= title]

-----------------------------------------------------------------------------
-- Metadata
-----------------------------------------------------------------------------

data Metadata = Metadata
  { bgg_username :: Text
  , game_collection :: [Game]
  , wishlist :: [Game]
  , preferences :: Prefs
  } deriving (Generic, Show, ToJSON)

-- Is it really possible to have missing fields?
instance FromJSON Metadata where
  parseJSON (Object o) = do
    bgg_username <- o .:? "bgg_username" .!= ""
    game_collection <- o .:? "game_collection" .!= []
    wishlist <- o .:? "wishlist" .!= []
    preferences <- o .:? "preferences" .!= defaultPreferences
    pure Metadata {bgg_username, game_collection, wishlist, preferences}
  parseJSON _ = mzero

defaultMetadata :: Metadata
defaultMetadata = Metadata
  { bgg_username = ""
  , game_collection = []
  , wishlist = []
  , preferences = defaultPreferences
  }

addGames :: [Game] -> Metadata -> Metadata
addGames games meta =
  let collection = nub $ game_collection meta ++ games in
  meta {game_collection = collection, wishlist = wishlist meta \\ collection}

getCustomerMetadata :: Auth -> CustomerId -> IO (Maybe Value)
getCustomerMetadata aut cid = do
  r <- try $ call aut "GET" ("/customers/" ++ show cid ++ "/metadata/") noData
  case r of
    -- XXX: we should distinguish 404 from other errors here
    Left (CurlAesonException {curlCode = CurlHttpReturnedError}) -> pure Nothing -- interpret as 404
    Left e -> do warn "unexpected exception using customer meta data end point"; throwIO e
    Right x -> pure x
  {-
    Wreq doesn't work due to HandshakeFailed from tls package (bug?)
    let opts = Network.Wreq.defaults & Network.Wreq.auth ?~ basicAuth (ByteString.Char8.pack user) (ByteString.Char8.pack pw)
    r <- Network.Wreq.getWith opts (cratejoyApiUrl ++ "/customers/" ++ show cid ++ "/metadata/")
    let results = r ^? responseBody . Data.Aeson.Lens.key "results"
    pure results
  -}

postCustomerMetadata :: ToJSON a => Auth -> CustomerId -> a -> IO ()
postCustomerMetadata aut cid x =
  call aut "POST" ("/customers/" ++ show cid ++ "/metadata/") (Just x)

-- | Given a customer, get its metadata from Cratejoy
getMetadata :: Auth -> Bool -> Customer -> IO Metadata
getMetadata aut warnIfNoMetadata c = do
  r <- getCustomerMetadata aut (id c)
  case r of
    Nothing -> do
      when warnIfNoMetadata $ warnCustomer c "no metadata for customer!"
      pure defaultMetadata
    Just x ->
      case parseMaybe (\case (Object o) -> o .: "data"; _ -> mzero) x of
        Nothing -> exit "Failed to parse customer meta data JSON!"
        Just meta -> pure meta

-----------------------------------------------------------------------------
-- Games file
-----------------------------------------------------------------------------

data Category
  = Popular
  | NewRelease
  | Forgotten
  deriving (Eq, Show)

instance Csv.FromField Category where
  parseField = \case
    "Popular" -> pure Popular
    "NewRelease" -> pure NewRelease
    "Forgotten" -> pure Forgotten
    _ -> mzero

-- Break out classification part?
data InventoryGame = InventoryGame
  { game :: Game
  , inventory :: !Int
  , category :: !Category
  , family :: !Bool
  , party :: !Bool
  , abstract :: !Bool
  , strategy :: !Bool
  , player2 :: !Bool
  , player3 :: !Bool
  } deriving Show

xbool :: Csv.Parser String -> Csv.Parser Bool
xbool p = p >>= \case "X" -> pure True; "" -> pure False; _ -> mzero

instance Csv.FromRecord InventoryGame where
  parseRecord v
    | length v == 10 = do
        game <- Game <$> v .! 0 <*> v .! 1
        InventoryGame game <$>
          v .! 2 <*>
          v .! 3 <*>
          xbool (v .! 4) <*>
          xbool (v .! 5) <*>
          xbool (v .! 6) <*>
          xbool (v .! 7) <*>
          xbool (v .! 8) <*>
          xbool (v .! 9)
    | otherwise =  mzero

readGames :: FilePath -> IO [InventoryGame]
readGames fp = do
  bs <- BL.readFile fp
  case Csv.decode Csv.HasHeader bs of
    Left msg -> exit ("CSV parse error: " <> Text.pack msg)
    Right v -> pure (Vector.toList v)

-----------------------------------------------------------------------------
-- Shipment file
-----------------------------------------------------------------------------

data ShipmentRecord = ShipmentRecord Customer Address Game deriving Show

instance Csv.DefaultOrdered ShipmentRecord where
  headerOrder _ = Csv.header
    [ "user_id"
    , "email"
    , "to"
    , "company"
    , "street"
    , "unit"
    , "city"
    , "zip_code"
    , "state"
    , "country"
    , "game_id"
    , "game_title"
    ]

instance Csv.ToNamedRecord ShipmentRecord where
  toNamedRecord (ShipmentRecord (Customer {..}) (Address {..}) (Game {..})) = Csv.namedRecord
    [ Csv.namedField "user_id" id
    , Csv.namedField "email" email
    , Csv.namedField "to" to
    , Csv.namedField "company" company
    , Csv.namedField "street" street
    , Csv.namedField "unit" unit
    , Csv.namedField "city" city
    , Csv.namedField "zip_code" zip_code
    , Csv.namedField "state" state
    , Csv.namedField "country" country
    , Csv.namedField "game_id" gameId
    , Csv.namedField "game_title" gameTitle
    ]

instance Csv.FromNamedRecord ShipmentRecord where
  parseNamedRecord m = do
    id <- Csv.lookup m "user_id"
    email <- Csv.lookup m "email"
    let customer = Customer email id ""
    to <- Csv.lookup m "to"
    company <- Csv.lookup m "company"
    street <- Csv.lookup m "street"
    unit <- Csv.lookup m "unit"
    city <- Csv.lookup m "city"
    zip_code <- Csv.lookup m "zip_code"
    state <- Csv.lookup m "state"
    country <- Csv.lookup m "country"
    let address = Address to company street unit city zip_code state country
    gameId <- Csv.lookup m "game_id"
    gameTitle <- Csv.lookup m "game_title"
    let game = Game {gameId, gameTitle}
    pure (ShipmentRecord customer address game)

toShipmentRecord :: (Customer, InventoryGame, Address) -> ShipmentRecord
toShipmentRecord (cust, igame, address) = ShipmentRecord cust address (game igame)

writeShipmentFile :: FilePath -> [(Customer, InventoryGame, Address)] -> IO ()
writeShipmentFile fp = BL.writeFile fp . Csv.encodeDefaultOrderedByName . map toShipmentRecord

readShipmentFile :: FilePath -> IO [ShipmentRecord]
readShipmentFile fp = do
  bs <- BL.readFile fp
  case Csv.decodeByName bs of
    Left msg -> exit ("Failed to decode shipment CSV file: " <> Text.pack msg)
    Right (_, v) -> pure $ Vector.toList v

updateCollection :: Auth -> ShipmentRecord -> IO ()
updateCollection aut (ShipmentRecord customer _ game) = do
  -- XXX: race condition
  meta <- getMetadata aut True customer
  let meta' = addGames [game] meta
  postCustomerMetadata aut (id customer) meta'

-----------------------------------------------------------------------------
-- Matching algorithm
-----------------------------------------------------------------------------

score :: Prefs -> InventoryGame -> Int
score prefs game =
  sum $ zipWith aspectScore
  [pPopular, pNewRelease, pForgotten, pFamily, pParty, pAbstract, pStrategy, p2Player, p3Player]
  [isCategory Popular, isCategory NewRelease, isCategory Forgotten, family, party, abstract, strategy, player2, player3]
  where
    aspectScore f b = let x = f prefs in if b game then x else 4 - x
    isCategory c g = c == category g

customerScores :: [InventoryGame] -> (Customer, Metadata) -> [(Customer, InventoryGame, Int)]
customerScores games (c, meta) = [(c, ig, score prefs ig) | ig <- games, notInCollection ig]
  where
    collection = map gameId (game_collection meta)
    notInCollection ig = gameId (game ig) `notElem` collection
    prefs = preferences meta

allocateGames :: [InventoryGame] -> [(Customer, InventoryGame, Int)] -> [(Customer, InventoryGame)]
allocateGames games rankings =
  let state = HashMap.fromList [(gameId (game ig), n) | ig <- games, let n = inventory ig, n > 0] in
  alloc state rankings

alloc :: HashMap ItemId Int -> [(Customer, InventoryGame, Int)] -> [(Customer, InventoryGame)]
alloc _ [] = []
alloc inventory ((customer, ig, _) : rest)
  | Just n <- HashMap.lookup (gameId (game ig)) inventory, n > 0 =
      let n' = max 0 (n-1)
          inventory' = HashMap.insert (gameId (game ig)) n' inventory
          rest' = filter (\(c,_,_) -> c /= customer) rest
      in
      (customer, ig) : alloc inventory' rest'
  | otherwise = alloc inventory rest

match :: [InventoryGame] -> [(Customer, Metadata)] -> [(Customer, InventoryGame)]
match games customers =
  let rankings = concatMap (customerScores games) customers in
  let sorted_rankings = sortBy (flip $ comparing (\(_,_,x) -> x)) rankings in
  allocateGames games sorted_rankings

-----------------------------------------------------------------------------
-- BGG game collections
-----------------------------------------------------------------------------

forUser :: (Monoid a, IsString a) => a -> a -> a
forUser msg username = msg <> fromString ", querying collection for user: " <> username

extractGame :: Text -> [Tag BL.ByteString] -> IO Game
extractGame username = \case
  (TagOpen "item" attrs : TagText _ : TagOpen "name" _ : TagText name : _)
    | Just x <- lookup "objectid" attrs,
      Just gameId <- readMay (BL.UTF8.toString x) ->
        pure Game {gameId, gameTitle = decodeUtf8 $ BL.toStrict name}
  _ -> exit ("Unexpected BGG response" `forUser` username)

extractGames :: Text -> BL.ByteString -> IO [Game]
extractGames username body = do
  let tags = parseTags body
  errors <- mapM message $ sections (~== ("<error>" :: String)) tags
  unless (null errors) (warn ("BGG API error: " <> Text.unlines errors `forUser` username))
  mapM (extractGame username) $ sections (~== ("<item>" :: String)) tags
  where
    text (_ : TagText str : _) = pure (Just str)
    text _ = do warn ("unexpected XML format" `forUser` username); pure Nothing
    message ts = do
      msgs <- mapM text $ sections (~== ("<message>" :: String)) ts
      pure (Text.unlines $ map (decodeUtf8 . BL.toStrict) $ catMaybes msgs)

queryBGG :: Text -> Int -> IO [Game]
queryBGG username 0 = do warn ("Giving up" `forUser` username); pure []
queryBGG username tries = do
  log $ "Querying BGG collection for user: " <> username
  x <- try $ Network.Wreq.get url
  case x of
    Left (HTTP.Client.StatusCodeException (HTTP.Types.Status code msg) _ _) -> do
      let m = "Error from BGG: " <> Text.pack (show code) <> " " <> decodeUtf8 msg
      exit (m `forUser` username)
    Left HTTP.Client.NoResponseDataReceived -> do
      info ("No response data received from BGG" `forUser` username)
      retry
    Left HTTP.Client.ResponseTimeout -> do
      info ("Response timeout from BGG" `forUser` username)
      retry
    Left e ->
      throwIO e
    Right r ->
      case r ^. responseStatus . statusCode of
        202 -> do
          log ("Access to BGG game collection accepted" `forUser` username)
          retry
        200 ->
          extractGames username (r ^. responseBody)
        _ -> do
          let msg = Text.pack $ show (r ^. responseStatus)
          warn ("BGG API returned error: " <> msg `forUser` username)
          pure []
  where
    -- BGG usernames may, for example, contain spaces
    escaped_username = escapeURIString isUnescapedInURIComponent (Text.unpack username)
    url = "http://www.boardgamegeek.com/xmlapi/collection/" ++ escaped_username ++ "?own=1"
    retry = do
      log "Waiting three seconds before resending request..."
      threadDelay 3000000
      queryBGG username (tries - 1)

getBGGCollection :: Text -> IO [Game]
getBGGCollection username = queryBGG username 10

updateUsernameAndCollection :: Auth -> Text -> Customer -> Metadata -> IO Metadata
updateUsernameAndCollection aut username customer meta = do
  -- there can be duplicates in the BGG collection
  bgg_collection <- nub <$> getBGGCollection username
  let additional_games = bgg_collection \\ game_collection meta
  if username == bgg_username meta && null additional_games then do
    info "Metadata already up-to-date for customer. Skipping."
    pure meta
  else do
    infoCustomer customer "Updating meta data for customer."
    let meta' = addGames additional_games meta {bgg_username = username}
    postCustomerMetadata aut (id customer) meta'
    pure meta'

refreshCollection :: Auth -> Customer -> Metadata -> IO Metadata
refreshCollection aut customer meta = do
  let username = bgg_username meta
  if username == "" then pure meta else
    updateUsernameAndCollection aut username customer meta

importUsernameAndCollection :: Auth -> HashMap Text Text -> Customer -> IO ()
importUsernameAndCollection aut hashmap customer = do
  meta <- getMetadata aut False customer
  case HashMap.lookup (name customer) hashmap of
    Just username | username /= "" -> void $ updateUsernameAndCollection aut username customer meta
    _ -> return () --info "No BGG username in CSV file, skipping customer."

-----------------------------------------------------------------------------
-- Logging
-----------------------------------------------------------------------------

log :: Text -> IO ()
log = Logging.log

info :: Text -> IO ()
info msg = do log msg; putStrLn (Text.unpack msg)

warn :: Text -> IO ()
warn msg = do Logging.warn msg; putStrLn ("Warning: " ++ Text.unpack msg)

xxxCustomer :: (Text -> IO ()) -> Customer -> Text -> IO ()
xxxCustomer f c msg = f (msg <> " Name: " <> name c <> " ID: " <> Text.pack (show (id c)))

warnCustomer :: Customer -> Text -> IO ()
warnCustomer = xxxCustomer warn

infoCustomer :: Customer -> Text -> IO ()
infoCustomer = xxxCustomer info

exit :: Text -> IO a
exit msg = do
  info ("Fatal error: " <> msg)
  die "Exiting."

-----------------------------------------------------------------------------
-- Commands
-----------------------------------------------------------------------------

doCustomers :: Auth -> IO ()
doCustomers aut = do
  customers <- getCustomers aut
  BL.writeFile "customers.csv" (Csv.encode customers)

doImport :: Auth -> FilePath -> IO ()
doImport aut fp = do
  log "Executing import command"
  info "Reading CSV file..."
  bs <- BL.readFile fp
  case Csv.decode Csv.NoHeader bs of
    Left msg -> exit ("CSV parse error: " <> Text.pack msg)
    Right v -> do
      let hashmap = HashMap.fromList (Vector.toList v)
      customers <- getCustomers aut
      info "Uploading BGG usernames and collections to Cratejoy..."
      mapM_ (importUsernameAndCollection aut hashmap) customers

doRefresh :: Auth -> IO [(Customer, Metadata)]
doRefresh aut = do
  log "Executing refresh command"
  customers <- getCustomers aut
  info ("Number of customers: " <> Text.pack (show (length customers)))
  subs <- getSubscriptions aut
  let stmap = HashMap.fromList [(id c, stat) | Subscription c stat <- subs]
  let active_customers = filter (\u -> isActive $ HashMap.lookupDefault "" (id u) stmap) customers
  info ("Number of active customers: " <> Text.pack (show (length active_customers)))
  info "Getting metadata..."
  customers_with_meta <- mapM (\c -> (c,) <$> getMetadata aut True c) active_customers
  info "Updating game collections from BGG..."
  mapM (\(c, m) -> (c,) <$> refreshCollection aut c m) customers_with_meta

doShipment :: Auth -> FilePath -> IO ()
doShipment aut fp = do
  log "Executing shipment command"
  info "Reading games CSV file..."
  games <- readGames fp
  customers <- doRefresh aut
  info "Matching and allocating games..."
  let allocated_games = match games customers
  if length allocated_games == length customers then do
    info "Getting customer addresses..."
    with_addresses <- mapM (\(u, g) -> do a <- getShippingAddress aut u; pure (u, g, a)) allocated_games
    info "Writing shipment.csv..."
    writeShipmentFile "shipment.csv" with_addresses
  else
    let customers_without = map fst customers \\ map fst allocated_games in
    exit ("Couldn't find games for all customers! Customers without games: " <> Text.pack (show customers_without))

doCommit :: Auth -> FilePath -> IO ()
doCommit aut fp = do
  log "Executing commit command"
  info "Reading shipment CSV file..."
  shipment <- readShipmentFile fp
  mapM_ (updateCollection aut) shipment
  print shipment

readConfig :: IO Auth
readConfig = do
  config <- Configurator.load [Configurator.Required "gbm-client.cfg"]
  username <- Configurator.require config "cratejoy.username"
  password <- Configurator.require config "cratejoy.password"
  pure (username, password)

main :: IO ()
main = Logging.withFileLogging "log.txt" $ do
  x <- cmdArgs interface
  aut <- readConfig
  case x of
    Customers -> doCustomers aut
    Import fp -> doImport aut fp
    Shipment fp -> doShipment aut fp
    Commit fp -> doCommit aut fp
    Refresh -> void $ doRefresh aut
