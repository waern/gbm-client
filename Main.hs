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
import Data.Aeson
import Data.Aeson.Lens
import Data.Csv ((.!))
import Data.Function hiding (id)
import Data.HashMap.Strict (HashMap)
import Data.List
import Data.Maybe
import Data.Monoid
import Data.Ord
import Data.String
import Data.String.Conv
import Data.Text (Text)
import GHC.Generics hiding (to)
import Network.URI
import Network.HTTP.Client
import Network.HTTP.Types.Status
import Prelude hiding (id, log)
import Safe
import System.Console.CmdArgs hiding (name)
import System.Exit
import System.IO
import Text.HTML.TagSoup
import qualified Control.Logging as Logging
import qualified Data.ByteString.Lazy as BL
import qualified Data.Configurator as Configurator
import qualified Data.Csv as Csv
import qualified Data.HashMap.Strict as HashMap
import qualified Data.Text as Text
import qualified Data.Vector as Vector
import qualified Network.HTTP.Client as HTTP.Client
import qualified Network.HTTP.Types as HTTP.Types
import qualified Network.Wreq as Wreq
import qualified Network.Wreq.Session as Wreq.Session

import Debug.Trace

-----------------------------------------------------------------------------
-- Command line interface
-----------------------------------------------------------------------------

data CLI
  = Customers
  | Import {users :: FilePath}
  | Shipment {games :: FilePath}
  | Commit {shipment :: FilePath}
  | Refresh
  deriving (Show, Data)

interface :: CLI
interface =
  modes [customers, imp, shipment, commit, refresh]
  &= summary "crateman v1.0"
  &= verbosity
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
type Env = (Auth, Wreq.Session.Session)

type ItemId = Int
type CustomerId = ItemId

--waern :: CustomerId
--waern = 53504949

cratejoyApiUrl :: String
cratejoyApiUrl = "https://api.cratejoy.com/v1"

opts :: Auth -> Wreq.Options
opts (user, pw) =
  Wreq.defaults & Wreq.auth ?~ Wreq.basicAuth (toS user) (toS pw)

post :: ToJSON a => Env -> String -> a -> IO ()
post (aut, sess) service x = do
  let uri = cratejoyApiUrl ++ service
  void $ Wreq.Session.postWith (opts aut) sess uri (encode x)

get :: Env -> String -> IO (Wreq.Response BL.ByteString)
get (aut, sess) service = do
  let uri = cratejoyApiUrl ++ service
  Wreq.Session.getWith (opts aut) sess uri

getCollection :: (FromJSON a, ToJSON a) => Env -> String -> IO (Maybe String, [a])
getCollection env service = do
  r <- get env service
  let next = r ^? Wreq.responseBody . key "next" . nonNull . _String
  let results = r ^? Wreq.responseBody . key "results" . _JSON
  results' <-
    case results of
      Nothing -> exit "failed to parse Cratejoy collection"
      Just rs -> pure rs
  pure (toS <$> next, results')

getFullCollection :: (FromJSON a, ToJSON a) => Env -> String -> IO [a]
getFullCollection env service = go [] ""
  where
    go acc page = do
      (next, l) <- getCollection env (service ++ page)
      let acc' = l ++ acc
      case next of
        Nothing -> pure acc'
        Just s -> do putChar '.'; go acc' s

-----------------------------------------------------------------------------
-- Customers
-----------------------------------------------------------------------------

data Customer = Customer {id :: CustomerId, email :: Text, name :: Text}
  deriving (Eq, Show, Generic, FromJSON, ToJSON)

instance Csv.ToRecord Customer

getCustomers :: Env -> IO [Customer]
getCustomers env =
  dots "Fetching customers from Cratejoy..." $
  getFullCollection env "/customers/"

-----------------------------------------------------------------------------
-- Subscriptions
-----------------------------------------------------------------------------

data Subscription = Subscription {customer :: Customer, status :: Text}
  deriving (Generic, Show, FromJSON, ToJSON)

isActive :: Text -> IO Bool
isActive = \case
  "active" -> pure True
  "renewing" -> pure True
  "unpaid" -> pure False
  "unpaid_order_failed" -> pure False
  "cancelled" -> pure False
  "suspended" -> pure False
  "expired" -> pure False
  "pastdue" -> pure False
  "past_due" -> pure False
  "pending_renewal" -> pure False
  s -> do warn ("unrecognized subscription status: " <> s); pure False

getSubscriptions :: Env -> IO [Subscription]
getSubscriptions env =
  dots "Fetching subscriptions from Cratejoy..." $
  getFullCollection env "/subscriptions/"

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
  } deriving (Generic, Show, FromJSON, ToJSON)

getCustomerAddresses :: Env -> CustomerId -> IO [Address]
getCustomerAddresses env cid =
  getFullCollection env ("/customers/" ++ show cid ++ "/addresses/")

getShippingAddress :: Env -> Customer -> IO Address
getShippingAddress env c = do
  l <- getCustomerAddresses env (id c)
  case l of
    []  -> exit ("No addresses registered for customer: " <> toS (show c))
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

getCustomerMetadata :: Env -> Bool -> Customer -> IO Metadata
getCustomerMetadata env warnIfNoMetadata cust = do
  x <- try $ get env ("/customers/" ++ show (id cust) ++ "/metadata/")
  case x of
    Left (StatusCodeException s _ _)
      | s ^. Wreq.statusCode == 404 -> do
         when warnIfNoMetadata $ warnCustomer cust "no metadata for customer!"
         pure defaultMetadata
    Left e -> throwIO e
    Right r ->
      case r ^? Wreq.responseBody . key "data" . _JSON of
        Nothing -> exit "failed to parse customer meta data response"
        Just meta -> pure meta

postCustomerMetadata :: ToJSON a => Env -> CustomerId -> a -> IO ()
postCustomerMetadata env cid x =
  post env ("/customers/" ++ show cid ++ "/metadata/") x

-----------------------------------------------------------------------------
-- Games file (inventory)
-----------------------------------------------------------------------------

data Category
  = Popular
  | Recent
  | Forgotten
  deriving (Eq, Show)

instance Csv.FromField Category where
  parseField = \case
    "Popular" -> pure Popular
    "Recent" -> pure Recent
    "Forgotten" -> pure Forgotten
    _ -> mzero

-- Break out classification part?
data InventoryGame = InventoryGame
  { game :: Game
  , inventory :: !Int
  , family :: !Bool
  , party :: !Bool
  , abstract :: !Bool
  , strategy :: !Bool
  , player2 :: !Bool
  , player3 :: !Bool
  , category :: !Category
  } deriving Show

xbool :: Csv.Parser String -> Csv.Parser Bool
xbool p = p >>= \case "X" -> pure True; "" -> pure False; _ -> mzero

instance Csv.FromRecord InventoryGame where
  parseRecord v
    | length v == 10 = do
        game <- Game <$> v .! 1 <*> v .! 0
        InventoryGame game <$>
          v .! 2 <*>
          xbool (v .! 3) <*>
          xbool (v .! 4) <*>
          xbool (v .! 5) <*>
          xbool (v .! 6) <*>
          xbool (v .! 7) <*>
          xbool (v .! 8) <*>
          v .! 9
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
    Left msg -> exit ("Failed to decode shipment CSV file: " <> toS msg)
    Right (_, v) -> pure $ Vector.toList v

updateCollection :: Env -> ShipmentRecord -> IO ()
updateCollection env (ShipmentRecord customer _ game) = do
  -- XXX: race condition
  meta <- getCustomerMetadata env True customer
  let meta' = addGames [game] meta
  postCustomerMetadata env (id customer) meta'

-----------------------------------------------------------------------------
-- Matching algorithm
-----------------------------------------------------------------------------

score :: Prefs -> InventoryGame -> Int
score prefs game =
  sum $ zipWith aspectScore
  [pPopular, pNewRelease, pForgotten, pFamily, pParty, pAbstract, pStrategy, p2Player, p3Player]
  [isCategory Popular, isCategory Recent, isCategory Forgotten, family, party, abstract, strategy, player2, player3]
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
forUser msg username = msg <> fromString ", when querying collection for user: " <> username

extractGame :: Text -> [Tag BL.ByteString] -> IO Game
extractGame username = \case
  (TagOpen "item" attrs : TagText _ : TagOpen "name" _ : TagText name : _)
    | Just x <- lookup "objectid" attrs,
      Just gameId <- readMay (toS x) -> pure Game {gameId, gameTitle = toS name}
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
    message :: [Tag BL.ByteString] -> IO Text
    message ts = do
      msgs <- mapM text $ sections (~== ("<message>" :: String)) ts
      pure (Text.unlines $ map toS $ catMaybes msgs)

queryBGG :: Text -> Int -> IO [Game]
queryBGG username 0 = do warn ("Giving up" `forUser` username); pure []
queryBGG username tries = do
  log $ "Querying BGG collection for user: " <> username
  x <- try $ Wreq.get url
  case x of
    Left (HTTP.Client.StatusCodeException (HTTP.Types.Status code msg) _ _) -> do
      let m = "Error from BGG: " <> toS (show code) <> " " <> toS msg
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
      case r ^. Wreq.responseStatus of
        st | st == status202 -> do
              log ("Access to BGG game collection accepted" `forUser` username)
              retry
           | st == ok200 ->
              extractGames username (r ^. Wreq.responseBody)
           | otherwise -> do
              let msg = toS $ show (r ^. Wreq.responseStatus)
              warn ("BGG API returned error: " <> msg `forUser` username)
              pure []
  where
    -- BGG usernames may, for example, contain spaces
    escaped_username = escapeURIString isUnescapedInURIComponent (toS username)
    url = "http://www.boardgamegeek.com/xmlapi/collection/" ++ escaped_username ++ "?own=1"
    retry = do
      log "Waiting three seconds before resending request..."
      threadDelay 3000000
      queryBGG username (tries - 1)

getBGGCollection :: Text -> IO [Game]
getBGGCollection username = queryBGG username 10

updateUsernameAndCollection :: Env -> Text -> Customer -> Metadata -> IO Metadata
updateUsernameAndCollection env username customer meta = do
  -- there can be duplicates in the BGG collection
  bgg_collection <- nub <$> getBGGCollection username
  let additional_games = bgg_collection \\ game_collection meta
  if username == bgg_username meta && null additional_games then
    --info "Metadata already up-to-date for customer. Skipping."
    pure meta
  else do
    infoCustomer customer "Updating meta data for customer."
    let meta' = addGames additional_games meta {bgg_username = username}
    postCustomerMetadata env (id customer) meta'
    pure meta'

refreshCollection :: Env -> Customer -> Metadata -> IO Metadata
refreshCollection env customer meta = do
  let username = bgg_username meta
  if username == "" then pure meta else
    updateUsernameAndCollection env username customer meta

importUsernameAndCollection :: Env -> HashMap Text Text -> Customer -> IO ()
importUsernameAndCollection env hashmap customer = do
  meta <- getCustomerMetadata env False customer
  case HashMap.lookup (name customer) hashmap of
    Just username | username /= "" -> void $ updateUsernameAndCollection env username customer meta
    _ -> return () --info "No BGG username in CSV file, skipping customer."

-----------------------------------------------------------------------------
-- Logging
-----------------------------------------------------------------------------

log :: Text -> IO ()
log = Logging.log

info :: Text -> IO ()
info msg = do log msg; putStrLn (toS msg)

warn :: Text -> IO ()
warn msg = do Logging.warn msg; putStrLn ("Warning: " ++ toS msg)

xxxCustomer :: (Text -> IO ()) -> Customer -> Text -> IO ()
xxxCustomer f c msg = f (msg <> " Name: " <> name c <> ", ID: " <> toS (show (id c)))

warnCustomer :: Customer -> Text -> IO ()
warnCustomer = xxxCustomer warn

infoCustomer :: Customer -> Text -> IO ()
infoCustomer = xxxCustomer info

exit :: Text -> IO a
exit msg = do
  info ("Fatal error: " <> msg)
  die "Exiting."

dots :: Text -> IO a -> IO a
dots msg x = do
  log msg
  putStr (toS msg)
  r <- x
  putChar '\n'
  pure r

-----------------------------------------------------------------------------
-- Commands
-----------------------------------------------------------------------------

doCustomers :: Env -> IO ()
doCustomers env = do
  customers <- getCustomers env
  BL.writeFile "customers.csv" (Csv.encode customers)

doImport :: Env -> FilePath -> IO ()
doImport env fp = do
  log "Executing import command"
  info "Reading CSV file..."
  bs <- BL.readFile fp
  case Csv.decode Csv.NoHeader bs of
    Left msg -> exit ("CSV parse error: " <> toS msg)
    Right v -> do
      let hashmap = HashMap.fromList (Vector.toList v)
      customers <- getCustomers env
      info "Uploading BGG usernames and collections to Cratejoy..."
      mapM_ (importUsernameAndCollection env hashmap) customers

doRefresh :: Env -> IO [(Customer, Metadata)]
doRefresh env = do
  log "Executing refresh command"
  customers <- getCustomers env
  info ("Number of customers: " <> toS (show (length customers)))
  subs <- getSubscriptions env
  let stmap = HashMap.fromListWith (++) [(id c, [stat]) | Subscription c stat <- subs]
  let active c = or <$> mapM isActive (HashMap.lookupDefault [] (id c) stmap)
  active_customers <- filterM active customers
  info ("Number of active customers: " <> toS (show (length active_customers)))
  info "Getting metadata..."
  customers_with_meta <- mapM (\c -> (c,) <$> getCustomerMetadata env True c) active_customers
  info "Updating game collections from BGG..."
  let actions = map (\(c,m) -> do m' <- refreshCollection env c m; pure (Just (c, m'))) customers_with_meta
  let actions' = intersperse (do threadDelay 1000000; pure Nothing) actions
  catMaybes <$> sequence actions'

doShipment :: Env -> FilePath -> IO ()
doShipment env fp = do
  log "Executing shipment command"
  info "Reading games CSV file..."
  games <- readGames fp
  customers <- doRefresh env
  info "Matching and allocating games..."
  let allocated_games = match games customers
  if length allocated_games == length customers then do
    with_addresses <-
      dots "Getting customer addresses..." $
      mapM (\(u, g) -> do a <- getShippingAddress env u; pure (u, g, a)) allocated_games
    info "Writing shipment.csv..."
    writeShipmentFile "shipment.csv" with_addresses
  else
    let customers_without = map fst customers \\ map fst allocated_games in
    exit ("Couldn't find games for all customers! Customers without games: " <> Text.pack (show customers_without))

doCommit :: Env -> FilePath -> IO ()
doCommit env fp = do
  log "Executing commit command"
  info "Reading shipment CSV file..."
  shipment <- readShipmentFile fp
  mapM_ (updateCollection env) shipment

readConfig :: IO Auth
readConfig = do
  config <- Configurator.load [Configurator.Required "gbm-client.cfg"]
  username <- Configurator.require config "cratejoy.username"
  password <- Configurator.require config "cratejoy.password"
  pure (username, password)

main :: IO ()
main = do
  hSetBuffering stdout NoBuffering
  Logging.withFileLogging "log.txt" $ do
    x <- cmdArgs interface
    aut <- readConfig
    Wreq.Session.withSession $ \sess ->
      let env = (aut, sess) in
      case x of
        Customers -> doCustomers env
        Import fp -> doImport env fp
        Shipment fp -> doShipment env fp
        Commit fp -> doCommit env fp
        Refresh -> void $ doRefresh env
