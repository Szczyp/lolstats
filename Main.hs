{-# LANGUAGE DeriveGeneric, FlexibleContexts, GeneralizedNewtypeDeriving,
             MultiParamTypeClasses, NoImplicitPrelude, OverloadedStrings, TemplateHaskell,
             TypeFamilies #-}

module Main where

import ClassyPrelude                   hiding (log, (<>))
import Control.Concurrent.Async.Lifted
import Control.Lens                    hiding (argument)
import Control.Monad.Base
import Control.Monad.Except            (ExceptT, MonadError, runExceptT, throwError)
import Control.Monad.State             (MonadState, StateT, runStateT)
import Control.Monad.Trans.Control
import Control.Monad.Writer            (MonadWriter, WriterT, runWriterT, tell)
import Data.Aeson                      (FromJSON, ToJSON, decode, encode)
import Data.Aeson.Lens
import Data.List                       (transpose)
import Data.Text                       (replace)
import Data.Tuple.Sequence
import Network.HTTP.Client             (HttpException (StatusCodeException))
import Network.Wreq                    (defaults, param, responseBody, statusMessage)
import Network.Wreq.Session            (Session, getWith, withSession)
import Options.Applicative
import System.Environment              (withArgs)
import Text.PrettyPrint.Boxes          hiding ((<>))

newtype AppError = AppError Text

instance Show AppError where
  show (AppError e) = "Error: " ++ unpack e

data ApiConfig = ApiConfig { apiKey :: Text } deriving (Show, Generic)
instance FromJSON ApiConfig

data Config = Config { cApi      :: ApiConfig
                     , cSession  :: Session
                     , cRegion   :: Text
                     , cPlatform :: Text
                     , cName     :: Text
                     } deriving (Show)

type SummonerCache = Map Text Integer

type ChampionCache = Map Text Text

data Cache = Cache { _cacheSummoners :: SummonerCache
                   , _cacheChampions :: ChampionCache }
             deriving (Show, Generic)
instance FromJSON Cache
instance ToJSON Cache
makeLenses ''Cache

emptyCache :: Cache
emptyCache = Cache mempty mempty

type Log = [Text]

newtype App a = App (ReaderT Config (StateT Cache (WriterT Log (ExceptT AppError IO))) a)
              deriving (Functor, Applicative, Monad, MonadIO, MonadReader Config
                       , MonadState Cache, MonadWriter Log, MonadError AppError, MonadBase IO)

instance MonadBaseControl IO App where
  type StM App a = Either AppError ((a, Cache), Log)
  liftBaseWith f = App $ liftBaseWith $ \r -> f (r . unApp)
    where unApp (App a) = a
  restoreM = App . restoreM

type GameInfo = [[(Text, Text, Text)]]

regionDict :: Map Text Text
regionDict = mapFromList [("na", "NA1"), ("euw", "EUW1"), ("eune", "EUN1"), ("kr", "KR")
                         ,("br", "BR1"), ("lan", "LA1"), ("las", "LA2"), ("ru", "RU")
                         ,("oce", "OC1"), ("tr", "TR1")]

txt :: Show a => a -> Text
txt = pack . show

throw :: MonadError AppError m => Text -> m a
throw = throwError . AppError

(???) :: Monad m => Maybe b -> m b -> m b
(???) a b = maybe b return a
infix 3 ???

(!??) :: Monad m => m (Maybe b) -> m b -> m b
(!??) a b = maybe b return =<< a
infix 3 !??

urlRoot :: Text -> Text
urlRoot region = "https://" ++ region ++ ".api.pvp.net/"

getData :: (MonadReader Config m, MonadWriter Log m, MonadError AppError m, MonadIO m)
           => Text -> m LByteString
getData url = do
  Config {cSession = sess, cApi = (ApiConfig aKey)} <- ask
  let opts = defaults & param "api_key" .~ [aKey]
  tell [url]
  view responseBody <$> getWith opts sess (unpack url) `catchHttp` handleError
  where handleError (StatusCodeException s _ _) = errorMsg . decodeUtf8 $ s ^. statusMessage
        handleError _ = errorMsg "Network Error"
        errorMsg msg = msg ++ " (" ++ url ++ ")"
        catchHttp act handler = liftIO ((Right <$> act) `catch` (return . Left . handler))
                                >>= either throw return

getSummonerId :: App Integer
getSummonerId = do
  (name, region) <- (cName &&& cRegion) <$> ask
  let url = urlRoot region ++ "api/lol/" ++ region ++ "/v1.4/summoner/by-name/" ++ name
      nameKey = key . replace " " "" $ name
      summonerCacheKey = region ++ " | " ++ name
      getId = preview $ nameKey . key "id" . _Integer
      fromCache = use $ cacheSummoners . at summonerCacheKey
      fromApi = getId <$> getData url !?? throw "Can't get summoner id"
  sid <- fromCache !?? fromApi
  cacheSummoners %= insertMap summonerCacheKey sid
  return sid

getCurrentGame :: Integer -> App GameInfo
getCurrentGame summonerId = do
  (region, platform) <- (cRegion &&& cPlatform) <$> ask
  let url = urlRoot region ++ "observer-mode/rest/consumer/getSpectatorGameInfo" ++ "/"
            ++ platform ++ "/" ++ txt summonerId
  participants <- getParticipants <$> getData url !?? throw "Illegal json"
  let getDivisions = getSummonersDivision $ map (view _2) participants
      getChampions = mapConcurrently (getChampionName . view _4) participants
  (divisions, champions) <- runConcurrently $ each Concurrently (getDivisions, getChampions)
  updateCache participants champions
  return $ map (map (\(c, n, d, _) -> (c, n, d)))
         $ groupAllOn (view _4)
         $ zipWith3 (\(n, _, t, _) d c -> (c, n, d, t)) participants divisions champions
  where getParticipants = mapM sequenceT . toListOf (key "participants" . values
                          . to ((,,,)
                                <$> preview (key "summonerName" . _String)
                                <*> preview (key "summonerId" . _Integer)
                                <*> preview (key "teamId" . _Integer)
                                <*> preview (key "championId" . _Integer)))
        updateCache p c = cacheChampions %= (++ mapFromList (zip (map (txt . view _4) p) c))

getSummonersDivision :: (MonadReader Config m, MonadWriter Log m, MonadError AppError m
                        , MonadIO m)
                        => [Integer] -> m [Text]
getSummonersDivision summonerIds = do
  region <- cRegion <$> ask
  let url = urlRoot region ++ "api/lol/" ++ region ++ "/v2.5/league/by-summoner/"
            ++ sids ++ "/entry"
  r <- getData url
  mapM (map getRankedSolo5x5 . getDivision r) summonerIds ??? throw "Illegal json"
  where sids = intercalate "," . map txt $ summonerIds
        getRankedSolo5x5 = maybe "Unranked" (\(_, t, d) -> t ++ " " ++ d)
                           . headMay . filter ((== "RANKED_SOLO_5x5") . view _1)
        getDivision json sid = mapM sequenceT $ json ^.. key (txt sid) . values
                               . to ((,,)
                                     <$> preview (key "queue" . _String)
                                     <*> preview (key "tier" . _String)
                                     <*> preview (key "entries" . nth 0 . key "division"
                                                  . _String))

getChampionName :: Integer -> App Text
getChampionName championId = use (cacheChampions . at (txt championId)) !?? getName
  where url = urlRoot "global" ++ "api/lol/static-data/eune/v1.2/champion/" ++ txt championId
        getName = preview (key "name" . _String) <$> getData url !?? throw "Illegal json"

parseApiConfig :: (MonadError AppError m, MonadIO m, MonadBaseControl IO m) => m ApiConfig
parseApiConfig = decode <$> readFile "config.json" `catchAny` fileError !?? parseError
  where fileError = const $ throw "Cannot find config file"
        parseError = throw "Cannot parse config file"

type Args = (Region, Summoner, Debug)
type Region = Text
type Summoner = Text
type Debug = Bool

parseArgs :: IO Args
parseArgs = execParser $ info (helper <*> parser) idm
  where parser = (,,) <$> arg "REGION" <*> arg "SUMMONER" <*> switch (long "debug")
        arg = map (toLower . pack) . strArgument . metavar

runApp :: App a -> Args -> Session -> IO (Either AppError a)
runApp (App app) (region, name, debug) sess = runExceptT $ do
  apiConfig <- parseApiConfig
  platform <- lookup region regionDict ??? throw ("unknown region " ++ region)
  let cfg = Config apiConfig sess region platform name
  cache <- readCache
  ((result, cache'), log) <- runWriterT (runStateT (runReaderT app cfg) cache)
  writeCache cache'
  when debug $ putStrLn (unlines log)
  return result
  where readCache = fromMaybe emptyCache . decode
                    <$> readFile "cache.json" `catchAny` const (return "")
        writeCache cache = writeFile "cache.json" (encode cache)
                           `catchAny` const (throw "Cannot write cache file (cache.json)")

mainApp :: App GameInfo
mainApp = getSummonerId >>= getCurrentGame

printGameInfo :: GameInfo -> IO ()
printGameInfo = printBox . hsep 6 left
                . map (hsep 2 left
                       . map (vcat left)
                       . transpose
                       . map (^.. each . to (text . unpack)))

main :: IO ()
main = do
  args <- parseArgs
  withSession (runApp mainApp args >=> either print printGameInfo)

lolstats :: String -> String -> IO ()
lolstats region summoner = withArgs [region, summoner] main
