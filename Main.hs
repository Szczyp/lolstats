{-# OPTIONS -fno-warn-name-shadowing #-}
{-# LANGUAGE DeriveGeneric, FlexibleContexts, GeneralizedNewtypeDeriving,
             MultiParamTypeClasses, NoImplicitPrelude, OverloadedStrings, TemplateHaskell,
             TypeFamilies #-}

module Main where

import ClassyPrelude                   hiding (log, (<>), Concurrently, throw, mapConcurrently
                                              , runConcurrently)
import Control.Concurrent.Async.Lifted
import Control.Lens                    hiding ((<.>), argument)
import Control.Monad.Except            (ExceptT, MonadError, runExceptT, throwError)
import Control.Monad.State             (MonadState, StateT, runStateT)
import Control.Monad.Writer            (MonadWriter, WriterT, runWriterT, tell)
import Data.Aeson                      (FromJSON, ToJSON, decode, encode)
import Data.Aeson.Lens
import Data.List                       (transpose)
import Data.Text                       (replace)
import Data.Tuple.Sequence
import Network.HTTP.Client             (HttpException (StatusCodeException))
import Network.Wreq                    (defaults, param, responseBody, statusMessage)
import Network.Wreq.Session            (Session, getWith, withAPISession)
import Options.Applicative
import System.Directory
import System.Environment              (withArgs)
import System.IO                       (hSetEncoding, utf8)
import Text.PrettyPrint.Boxes          hiding ((<>))

newtype AppError = AppError Text

instance Show AppError where
  show (AppError e) = "Error: " ++ unpack e

data ApiConfig = ApiConfig { apiKey :: Text } deriving (Show, Generic)
instance FromJSON ApiConfig

data Config = Config { api      :: ApiConfig
                     , session  :: Session
                     , region   :: Text
                     , platform :: Text
                     , name     :: Text
                     } deriving (Show)

type SummonerCache = Map Text Integer

type ChampionCache = Map Text Text

data Cache = Cache { _summoners :: SummonerCache
                   , _champions :: ChampionCache }
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

class Monad m => MonadHttp m where
  httpGet :: Text -> m LByteString

instance MonadHttp App where
  httpGet url = do
    Config {session = session, api = (ApiConfig aKey)} <- ask
    let opts = defaults & param "api_key" .~ [aKey]
    tell [url]
    view responseBody <$> getWith opts session (unpack url) `catchHttp` handleError
    where handleError (StatusCodeException s _ _) = errorMsg . decodeUtf8 $ s ^. statusMessage
          handleError _ = errorMsg "Network Error"
          errorMsg msg = msg ++ " (" ++ url ++ ")"
          catchHttp action handler = liftIO ((Right <$> action) `catch` (return . Left . handler))
                                  >>= either throw return

type GameInfo = [[(Text, Text, Text, Text)]]

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

getSummonerId :: (MonadReader Config m, MonadError AppError m, MonadState Cache m, MonadHttp m)
              => m Integer
getSummonerId = do
  (name, region) <- (name &&& region) <$> ask
  let url = urlRoot region ++ "api/lol/" ++ region ++ "/v1.4/summoner/by-name/" ++ name
      nameKey = key . replace " " "" $ name
      summonerCacheKey = region ++ " | " ++ name
      getId = preview $ nameKey . key "id" . _Integer
      fromCache = use $ summoners . at summonerCacheKey
      fromApi = getId <$> httpGet url !?? throw "Can't get summoner id"
  sid <- fromCache !?? fromApi
  summoners %= insertMap summonerCacheKey sid
  return sid

getCurrentGame :: (MonadReader Config m, MonadState Cache m, MonadError AppError m, MonadHttp m
                  , MonadBaseControl IO m)
               => Integer -> m GameInfo
getCurrentGame summonerId = do
  (region, platform) <- (region &&& platform) <$> ask
  let url = urlRoot region ++ "observer-mode/rest/consumer/getSpectatorGameInfo" ++ "/"
            ++ platform ++ "/" ++ txt summonerId
  participants <- getParticipants <$> httpGet url !?? throw "Illegal json"
  let getDivisions = getSummonersDivision $ map (view _2) participants
      getChampions = mapConcurrently (getChampionName . view _4) participants
  (divisions, champions) <- runConcurrently $ (,)
                            <$> Concurrently getDivisions
                            <*> Concurrently getChampions
  updateCache participants champions
  return $ map (map (\(c, n, d, s, _) -> (c, n, d, s)))
         $ groupAllOn (view _5)
         $ zipWith3 (\(n, _, t, _) (d, s) c -> (c, n, d, s, t)) participants divisions champions
  where getParticipants = mapM sequenceT . toListOf (key "participants" . values
                          . to ((,,,)
                                <$> preview (key "summonerName" . _String)
                                <*> preview (key "summonerId" . _Integer)
                                <*> preview (key "teamId" . _Integer)
                                <*> preview (key "championId" . _Integer)))
        updateCache p c = champions %= (++ mapFromList (zip (map (txt . view _4) p) c))

getSummonersDivision :: (MonadReader Config m, MonadError AppError m, MonadHttp m)
                     => [Integer] -> m [(Text, Text)]
getSummonersDivision summonerIds = do
  region <- region <$> ask
  let url = urlRoot region ++ "api/lol/" ++ region ++ "/v2.5/league/by-summoner/"
            ++ sids ++ "/entry"
  r <- httpGet url
  mapM (map getRankedSolo5x5 . getDivision r) summonerIds ??? throw "Illegal json"
  where sids = intercalate "," . map txt $ summonerIds
        getRankedSolo5x5 = maybe ("Unranked", "") (formatDivision &&& formatSeries)
                           . headMay . filter ((== "RANKED_SOLO_5x5") . view _1)
        getDivision json sid = mapM (sequenceT . over _5 (Just . fromMaybe ""))
                               $ json ^.. key (txt sid) . values
                               . to ((,,,,)
                                     <$> preview (key "queue" . _String)
                                     <*> preview (key "tier" . _String)
                                     <*> preview (key "entries" . nth 0
                                                  . key "division" . _String)
                                     <*> preview (key "entries" . nth 0
                                                  . key "leaguePoints" . _Integer)
                                     <*> preview (key "entries" . nth 0
                                                  . key "miniSeries" . key "progress" . _String))
        formatDivision (_, tier, division, _, _) = tier ++ " " ++ division
        formatSeries (_, _, _, lps, series) =  if lps == 100 then series else txt lps

getChampionName :: (MonadState Cache m, MonadError AppError m, MonadHttp m)
                => Integer -> m Text
getChampionName championId = use (champions . at (txt championId)) !?? getName
  where url = urlRoot "global" ++ "api/lol/static-data/eune/v1.2/champion/" ++ txt championId
        getName = preview (key "name" . _String) <$> httpGet url !?? throw "Illegal json"

parseApiConfig :: (MonadCatch m, MonadError AppError m, MonadIO m) => FilePath -> m ApiConfig
parseApiConfig path = decode <$> readFile path `catchAny` fileError !?? parseError
  where fileError = const $ throw "Cannot find config file"
        parseError = throw "Cannot parse config file"

type Args = (Region, Summoner, Debug)
type Region = Text
type Summoner = Text
type Debug = Bool

parseArgs :: IO Args
parseArgs = execParser $ info (helper <*> parser) idm
  where parser = (,,) <$> arg "REGION" <*> arg "SUMMONER" <*> switch (long "debug")
        arg = map (toLower . replace " " "" . pack) . strArgument . metavar

runApp :: App a -> Args -> Session -> IO (Either AppError a)
runApp (App app) (region, name, debug) session = runExceptT $ do
  let appName = "lolstats"
  dir <- liftIO $ getAppUserDataDirectory appName
  let configPath = dir </> appName <.> "config"
      cachePath = dir </> appName <.> "cache"
      readCache = fromMaybe emptyCache . decode
                  <$> readFile cachePath `catchAny` const (return "")
      writeCache cache = writeFile cachePath (encode cache)
                         `catchAny` const (throw "Cannot write cache file")
  apiConfig <- parseApiConfig configPath
  platform <- lookup region regionDict ??? throw ("unknown region " ++ region)
  let cfg = Config apiConfig session region platform name
  ((result, cache), log) <- runWriterT (runStateT (runReaderT app cfg) =<< readCache)
  writeCache cache
  when debug $ putStrLn (unlines log)
  return result

mainApp :: App GameInfo
mainApp = getSummonerId >>= getCurrentGame

printGameInfo :: GameInfo -> IO ()
printGameInfo = printBox . hsep 6 left
                . map (hsep 2 left
                       . map (vcat left)
                       . transpose
                       . map (^.. each . to (text . unpack))
                       . (++) [("CHAMPION:", "SUMMONER:", "DIVISION:", "LP:")])

main :: IO ()
main = do
  mapM_ (`hSetEncoding` utf8) [stdout, stderr, stdin]
  args <- parseArgs
  withAPISession (runApp mainApp args >=> either print printGameInfo)

lolstats :: String -> String -> IO ()
lolstats region summoner = withArgs [region, summoner] main
