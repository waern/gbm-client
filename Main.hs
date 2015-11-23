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
import Data.Scientific
import Data.Text (Text)
import Data.Time
import Data.Time.Format
import Data.Time.Clock.POSIX
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
import qualified Data.Aeson as Aeson
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
import Network.Curl

-----------------------------------------------------------------------------
-- Command line interface
-----------------------------------------------------------------------------

data CLI
  = Customers
  | Import {users :: FilePath}
  | Shipment {games :: FilePath, t1 :: String, t2 :: String}
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
    shipment = Shipment {games = def &= argPos 0 &= typFile, t1 = def &= argPos 2, t2 = def &= argPos 3} &= help "Make shipment.csv, taking game inventory CSV file as input"
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
  Wreq.defaults
    & Wreq.auth ?~ Wreq.basicAuth (toS user) (toS pw)
    & Wreq.manager . _Left %~ (\ms -> ms {managerResponseTimeout = Just (120 * 1000000)})

get :: Env -> String -> IO (Wreq.Response BL.ByteString)
get (aut, sess) service = do
  let uri = cratejoyApiUrl ++ service
  Wreq.Session.getWith (opts aut) sess uri

-- Using CURL here as increasing the timeout with Wreq
-- doesn't work for some reason
getCurl :: (FromJSON a, ToJSON a) => Env -> String -> IO (Maybe String, [a])
getCurl ((user,pwd), _) url = do
  (code, str) <- curlGetString url [CurlUserPwd (user ++ ":" ++ pwd)]
  case code of
    CurlOK ->
      case decode (toS str) of
        Nothing -> exit "failed to decode cratejoy JSON response"
        Just (r :: Aeson.Value) -> do
          let next = r ^? key "next" . nonNull . _String
          results <-
            case Aeson.fromJSON (fromJust (r ^? key "results")) of
              Error err -> exit ("Failed to prase cratejoy JSON response: " <> toS err)
              Success r -> pure r
          pure (toS <$> next, results)
    _ -> exit (toS $ show code)

getFullCollectionCurl :: (FromJSON a, ToJSON a) => Env -> String -> IO [a]
getFullCollectionCurl env service = go [] 1
  where
    go acc n = do
      let url = if n == 1 then service else service ++ "&page=" ++ show n
      (next, l) <- getCurl env url
      let acc' = l ++ acc
      case next of
        Nothing -> pure acc'
        Just _ -> do putChar '.'; go acc' (n + 1)

post :: ToJSON a => Env -> String -> a -> IO ()
post (aut, sess) service x = do
  let uri = cratejoyApiUrl ++ service
  void $ Wreq.Session.postWith (opts aut) sess uri (encode x)

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
getFullCollection env service = go [] 0
  where
    go acc n = do
      let url = if n == 0 then service else service ++ "?page=" ++ show n
      (next, l) <- getCollection env url
      let acc' = l ++ acc
      case next of
        Nothing -> pure acc'
        Just _ -> do putChar '.'; go acc' (n + 1)

-----------------------------------------------------------------------------
-- Customers
-----------------------------------------------------------------------------

data Customer = Customer {id :: CustomerId, email :: Text, name :: Text}
  deriving (Ord, Eq, Show, Generic, FromJSON, ToJSON)

instance Csv.ToRecord Customer

getCustomers :: Env -> IO [Customer]
getCustomers env =
  dots "Fetching customers from Cratejoy..." $
  getFullCollection env "/customers/"

-----------------------------------------------------------------------------
-- Subscriptions
-----------------------------------------------------------------------------

data Subscription = Subscription
  { subId :: ItemId
  , customer :: Customer
  , status :: Text
  }
  deriving (Generic, Show, ToJSON)

instance FromJSON Subscription where
  parseJSON (Object v) = Subscription <$> v .: "id" <*> v .: "customer" <*> v .: "status"
  parseJSON _ = mzero

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
  } deriving (Generic, Ord, Eq, Show, FromJSON, ToJSON)

getCustomerAddresses :: Env -> CustomerId -> IO [Address]
getCustomerAddresses env cid =
  getFullCollection env ("/customers/" ++ show cid ++ "/addresses/")

{-
getShippingAddress :: Env -> Customer -> IO Address
getShippingAddress env c = do
  l <- getCustomerAddresses env (id c)
  case l of
    []  -> exit ("No addresses registered for customer: " <> toS (show c))
    [a] -> pure a
    a : _ -> do warnCustomer c "more than one address for customer!"; pure a
-}

-----------------------------------------------------------------------------
-- Shipments
-----------------------------------------------------------------------------

newtype CratejoyDateTime = CratejoyDateTime UTCTime
  deriving (Generic, Ord, Eq, Show, ToJSON)

msSinceEpochToUtc :: Int -> UTCTime
msSinceEpochToUtc = posixSecondsToUTCTime . fromInteger . (`div` 1000) . toInteger

instance FromJSON CratejoyDateTime where
  parseJSON (Number n) =
    case floatingOrInteger n of
      Left _ -> error "Unexpected date format"
      Right i -> pure . CratejoyDateTime . msSinceEpochToUtc $ i
  parseJSON _ = mzero

data Instance = Instance {product_id :: ItemId}
  deriving (Generic, Ord, Eq, Show, ToJSON, FromJSON)

data Fulfillment = Fulfillment {fulInstance :: Instance}
  deriving (Generic, Ord, Eq, Show, ToJSON)

instance FromJSON Fulfillment where
  parseJSON (Object v) = Fulfillment <$> v .: "instance"
  parseJSON _ = mzero

data Ship = Ship
  { --shipAdjustedOrderedAt :: CratejoyDateTime
    shipId :: ItemId
  , shipCustomer :: Customer
  , shipStatus :: Text
  , shipIsTest :: Bool
  , shipAddress :: Address
  , shipFulfillments :: [Fulfillment]
  }
  deriving (Generic, Ord, Show, ToJSON)

instance Eq Ship where
  (==) = (==) `on` shipId

instance FromJSON Ship where
  parseJSON (Object v) =
    Ship <$>
 --     v .: "adjusted_ordered_at" <*>
      v .: "id" <*>
      v .: "customer" <*>
      v .: "status" <*>
      v .: "is_test" <*>
      v .: "ship_address" <*>
      v .: "fulfillments"
  parseJSON _ = mzero

data X = Full | Morning | Evening

getShipmentsOn :: Env -> X -> Day -> IO [Ship]
getShipmentsOn env x d = do
  let f day = formatTime defaultTimeLocale (iso8601DateFormat Nothing) (UTCTime day 0)
  let pad = \case [c] -> '0':[c]; s -> s
  let range = case x of Full -> [0..23]; Morning -> [0..12]; Evening -> [12..23]
  let times = ["T" ++ pad (show h) ++ ":00:00Z" | h <- range]
  let call d1 t1 d2 t2 = do
        let s1 = f d1 ++ t1
        let s2 = f d2 ++ t2
        let query = "adjusted_ordered_at__ge=" ++ s1 ++ "&adjusted_ordered_at__lt=" ++ s2
        snd <$> getCollection env ("/shipments/?limit=300&" ++ query)
  let
    last = case x of Morning -> d; _ -> addDays 1 d
    go [t] = [call d t last "T00:00:00Z"]
    go (t1:t2:ts) = call d t1 d t2 : go (t2 : ts)
  concat <$> sequence (go times)

daysBetween :: Day -> Day -> [Day]
daysBetween d1 d2 =
  let d1' = min d1 d2 in
  let d2' = max d1 d2 in
  go d1' d2'
  where
    go a b
      | a < b = a : daysBetween (addDays 1 a) b
      | otherwise = [a]

getShipments :: Env -> String -> String -> IO [Ship]
getShipments env d1 d2 = do
  {-
  info "Fetching shipments from Cratejoy..."
  let days = daysBetween d1 d2
  concat <$> mapM (getShipmentsOn env Full) days
  -}
  --let f day = formatTime defaultTimeLocale (iso8601DateFormat Nothing) (UTCTime day 0)
  let s1 = d1 ++ "T00:00:00Z"
  let s2 = d2 ++ "T00:00:00Z"
  let query = "adjusted_ordered_at__ge=" ++ s1 ++ "&adjusted_ordered_at__lt=" ++ s2
  info "Fetching shipments from Cratejoy..."
  getFullCollectionCurl env (cratejoyApiUrl ++ "/shipments/?with=customer,fulfillments&" ++ query)
  --dots "Fetching shipments from Cratejoy..." $
  --  getFullCollection env ("/shipments/?limit=10000&" ++ query)
  {-
  case days of
    d : ds -> do
      ships <- getShipmentsOn env Full d
      shipss <- mapM (getShipmentsOn env Full) (init ds)
      lastships <- getShipmentsOn env Full (last ds)
      return (ships ++ concat shipss ++ lastships)
    _ -> error "Bigger interval needed"
  -}

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

getCustomerMetadata :: Env -> Bool -> CustomerId -> IO Metadata
getCustomerMetadata env warnIfNoMetadata cid = do
  x <- try $ get env ("/customers/" ++ show cid ++ "/metadata/")
  case x of
    Left (StatusCodeException s _ _)
      | s ^. Wreq.statusCode == 404 -> do
         when warnIfNoMetadata $ warnCustomer cid "no metadata for customer!"
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
    "Must Haves" -> pure Popular
    "Recent Releases" -> pure Recent
    "Hidden Gems" -> pure Forgotten
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
xbool p =
  p >>= \case
    "X" -> pure True
    "x" -> pure True
    "" -> pure False
    _ -> mzero

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
-- Cratejoy subscription export
-----------------------------------------------------------------------------

data SubscriptionExport = SubscriptionExport
  { expId :: Int
  , expStatus :: String }
  deriving Show

instance Csv.FromNamedRecord SubscriptionExport where
  parseNamedRecord m = do
    id <- Csv.lookup m "id"
    status <- Csv.lookup m "status"
    pure (SubscriptionExport id status)

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

toShipmentRecord :: (Ship, Game) -> ShipmentRecord
toShipmentRecord (ship, g) = ShipmentRecord (shipCustomer ship) (shipAddress ship) g

writeShipmentFile :: FilePath -> [(Ship, Game)] -> IO ()
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
  let cid = id customer
  meta <- getCustomerMetadata env True cid
  let meta' = addGames [game] meta
  postCustomerMetadata env cid meta'

-----------------------------------------------------------------------------
-- Cratejoy shipment file
-----------------------------------------------------------------------------

{-
data CShipmentRecord = CShipmentRecord {cshipId :: Int, cshipCustId :: Int}
  deriving Show

instance Csv.FromNamedRecord CShipmentRecord where
  parseNamedRecord m = do
    id <- Csv.lookup m "id"
    custId <- Csv.lookup m "customer_id"
    pure (CShipmentRecord id custId)

readCShipmentFile :: FilePath -> IO [CShipmentRecord]
readCShipmentFile fp = do
  bs <- BL.readFile fp
  case Csv.decodeByName bs of
    Left msg -> exit ("Failed to decode shipment CSV file: " <> toS msg)
    Right (_, v) -> pure $ Vector.toList v
-}

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

customerScores :: [InventoryGame] -> (Ship, Metadata) -> [(Ship, InventoryGame, Int)]
customerScores games (ship, meta) = [(ship, ig, score prefs ig) | ig <- games, notInCollection ig]
  where
    collection = map gameId (game_collection meta)
    notInCollection ig = gameId (game ig) `notElem` collection
    prefs = preferences meta

allocateGames :: [InventoryGame] -> [(Ship, InventoryGame, Int)] -> [(Ship, InventoryGame)]
allocateGames games rankings =
  let state = HashMap.fromList [(gameId (game ig), n) | ig <- games, let n = inventory ig, n > 0] in
  alloc state rankings

alloc :: HashMap ItemId Int -> [(Ship, InventoryGame, Int)] -> [(Ship, InventoryGame)]
alloc _ [] = []
alloc inventory ((shipment, ig, _) : rest)
  | Just n <- HashMap.lookup (gameId (game ig)) inventory, n > 0 =
      let n' = max 0 (n-1)
          inventory' = HashMap.insert (gameId (game ig)) n' inventory
          rest' = filter (\(ship,_,_) -> ship /= shipment) rest
      in
      (shipment, ig) : alloc inventory' rest'
  | otherwise = alloc inventory rest

match :: [InventoryGame] -> [(Ship, Metadata)] -> [(Ship, InventoryGame)]
match games shipments =
  let rankings = concatMap (customerScores games) shipments in
  let sorted_rankings = sortBy (flip $ comparing (\(_,_,x) -> x)) rankings in
  allocateGames games sorted_rankings

-----------------------------------------------------------------------------
-- BGG game collections
-----------------------------------------------------------------------------

forUser :: (Monoid a, IsString a) => a -> a -> a
forUser msg username = msg <> fromString ", when querying collection for user: " <> username

extractGame :: Text -> [Tag BL.ByteString] -> IO (Maybe Game)
extractGame username = \case
  (TagOpen "item" attrs : TagText _ : TagOpen "name" _ : TagText name : rest)
    | Just x <- lookup "objectid" attrs,
      Just gameId <- readMay (toS x),
      (TagOpen "status" statusAttrs : _) : _ <- sections (~== ("<status>" :: String)) rest,
      Just own <- lookup "own" statusAttrs
      -> if own == "1" then pure (Just (Game {gameId, gameTitle = toS name})) else pure Nothing
  _ -> exit ("Unexpected BGG response" `forUser` username)

extractGames :: Text -> BL.ByteString -> IO [Game]
extractGames username body = do
  let tags = parseTags body
  errors <- mapM message $ sections (~== ("<error>" :: String)) tags
  unless (null errors) (warn ("BGG API error: " <> Text.unlines errors `forUser` username))
  catMaybes <$> (mapM (extractGame username) $ sections (~== ("<item>" :: String)) tags)
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
    Left (HTTP.Client.InternalIOException e) -> do
      info (toS (show e) `forUser` username)
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

updateUsernameAndCollection :: Env -> Text -> CustomerId -> Metadata -> IO Metadata
updateUsernameAndCollection env username cid meta = do
  -- there can be duplicates in the BGG collection
  bgg_collection <- nub <$> getBGGCollection username
  let additional_games = bgg_collection \\ game_collection meta
  if username == bgg_username meta && null additional_games then
    --info "Metadata already up-to-date for customer. Skipping."
    pure meta
  else do
    infoCustomer cid "Updating meta data for customer."
    let meta' = addGames additional_games meta {bgg_username = username}
    postCustomerMetadata env cid meta'
    pure meta'

refreshCollection :: Env -> CustomerId -> Metadata -> IO Metadata
refreshCollection env cid meta = do
  let username = bgg_username meta
  if username == "" then pure meta else
    updateUsernameAndCollection env username cid meta

importUsernameAndCollection :: Env -> HashMap Text Text -> Customer -> IO ()
importUsernameAndCollection env hashmap customer = do
  let cid = id customer
  meta <- getCustomerMetadata env False cid
  case HashMap.lookup (name customer) hashmap of
    Just username | username /= "" -> void $ updateUsernameAndCollection env username cid meta
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

xxxCustomer :: (Text -> IO ()) -> CustomerId -> Text -> IO ()
xxxCustomer f cid msg = f (msg <> " ID: " <> toS (show cid))

warnCustomer :: CustomerId -> Text -> IO ()
warnCustomer = xxxCustomer warn

infoCustomer :: CustomerId -> Text -> IO ()
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

doRefresh :: Env -> IO [(CustomerId, Metadata)]
doRefresh env = do
  log "Executing refresh command"
  customers <- getCustomers env
  info ("Number of customers: " <> toS (show (length customers)))
  subs <- getSubscriptions env
  info ("Number of subscriptions: " <> toS (show (length subs)))
  nb_active_subs <- length <$> filterM (\x -> isActive (status x)) subs
  info ("Number of active subscriptions: " <> toS (show nb_active_subs))
  let stmap = HashMap.fromListWith (++) [(id c, [stat]) | Subscription _ c stat <- subs]
  let active c = or <$> mapM isActive (HashMap.lookupDefault [] (id c) stmap)
  active_customers <- filterM active customers
  info ("Number of active customers: " <> toS (show (length active_customers)))
  info "Getting metadata..."
  let customer_ids = map id active_customers
  customers_with_meta <- mapM (\id -> (id,) <$> getCustomerMetadata env True id) customer_ids
  info "Updating game collections from BGG..."
  let actions = map (\(id,m) -> do m' <- refreshCollection env id m; pure (Just (id, m'))) customers_with_meta
  let actions' = intersperse (do threadDelay 1000000; pure Nothing) actions
  catMaybes <$> sequence actions'

doShipment :: Env -> FilePath -> String -> String -> IO ()
doShipment env fp t1 t2 = do
  log "Executing shipment command"
  {-
  let f = utctDay . parseTimeOrError True defaultTimeLocale (iso8601DateFormat Nothing)
  let (d1, d2) = (f t1, f t2)
  -}
  info "Reading games CSV file..."
  games <- readGames fp
  shipments <- getShipments env t1 t2
  let shipments' = sort shipments
  let shipments'' = filter (not . shipIsTest) shipments'
  let gameboxes = [19988034, 759409]
  let shipments''' = filter (any (`elem` gameboxes) . map (product_id . fulInstance) . shipFulfillments) shipments''
  info ("Number of shipments: " <> toS (show (length shipments''')))
  {-
  stuff <- readCShipmentFile "cshipments.csv"
  let theirs = map (\x -> (cshipId x, cshipCustId x)) stuff
  let ours = map (\x -> (shipId x, id (shipCustomer x))) shipments''
  let extra = ours \\ theirs
  print extra
  -}
  info "Getting metadata..."
  let customer_ids = nub $ map (id . shipCustomer) shipments'''
  customers_with_meta <- mapM (\id -> (id,) <$> getCustomerMetadata env True id) customer_ids
  info "Updating game collections from BGG..."
  let actions = map (\(id,m) -> do m' <- refreshCollection env id m; pure (Just (id, m'))) customers_with_meta
  let actions' = intersperse (do threadDelay 1000000; pure Nothing) actions
  customers_with_meta' <- catMaybes <$> sequence actions'
  let meta_map = HashMap.fromList customers_with_meta'
  let ship_meta = [ (ship, fromJust $ HashMap.lookup (id (shipCustomer ship)) meta_map) | ship <- shipments''']
  info "Matching and allocating games..."
  let allocated_games = match games ship_meta
  let allocated_games' = map (\(ship, igame) -> (ship, game igame)) allocated_games
  shipments <-
    if length allocated_games' == length ship_meta then
      return allocated_games'
    else do
      let without = map fst ship_meta \\ map fst allocated_games'
      let ids = map shipId (map fst ship_meta \\ map fst allocated_games')
      warn ("Couldn't find games for all shipments! Shipments without games: " <> Text.pack (show ids))
      return (allocated_games' ++ map (\ship -> (ship, Game (-1) "NO MATCH")) without)
  info "Writing shipment.csv..."
  writeShipmentFile "shipment.csv" shipments

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
        Shipment fp t1 t2 -> doShipment env fp t1 t2
        Commit fp -> doCommit env fp
        Refresh -> void $ doRefresh env
