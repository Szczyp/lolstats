{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TypeFamilies               #-}

module Main where

import           ClassyPrelude                   hiding ((<>))
import           Control.Concurrent.Async.Lifted
import           Control.Lens                    hiding (argument)
import           Control.Monad.Base
import           Control.Monad.Except            (ExceptT, MonadError,
                                                  runExceptT, throwError)
import           Control.Monad.State             (MonadState, StateT, get,
                                                  modify, runStateT)
import           Control.Monad.Trans.Control
import           Data.Aeson                      (FromJSON, decode, encode)
import           Data.Aeson.Lens
import           Data.Text                       (replace)
import           Network.HTTP.Client             (HttpException (StatusCodeException))
import           Network.Wreq                    (Response, defaults, param,
                                                  responseBody, statusMessage)
import           Network.Wreq.Session            (Session, getWith, withSession)
import           Options.Applicative
import           Text.Show.Pretty                (ppShow)

newtype App a = App (ReaderT Config (StateT Cache (ExceptT AppError IO)) a)
              deriving (Functor, Applicative, Monad, MonadIO, MonadReader Config
                       , MonadState Cache, MonadError AppError, MonadBase IO)

instance MonadBaseControl IO App where
  type StM App a = Either AppError (a, Cache)
  liftBaseWith f = App $ liftBaseWith $ \r -> f (r . unApp)
    where unApp (App a) = a
  restoreM = App . restoreM

newtype AppError = AppError Text

instance Show AppError where
  show (AppError e) = "Error: " ++ unpack e

data ApiConfig = ApiConfig { apiKey :: Text } deriving (Show, Generic)
instance FromJSON ApiConfig

data Config = Config { cApi     :: ApiConfig
                     , cSession :: Session
                     , cRegion  :: Text
                     , cName    :: Text
                     } deriving (Show)

type Cache = Map Text Text

type GameInfo = [[(Text, Text, Text)]]

txt :: Show a => a -> Text
txt = pack . show

throw :: MonadError AppError m => Text -> m a
throw = throwError . AppError

runApp :: Show a => App a -> Config -> Cache -> ExceptT AppError IO (a, Cache)
runApp (App a) c = runStateT (runReaderT a c)

catchHttp :: IO a -> (HttpException -> Text) -> App a
catchHttp act handler = liftIO (catch (Right <$> act) (return . Left . handler))
                           >>= either throw return

urlRoot :: Text -> Text
urlRoot region = "https://" ++ region ++ ".api.pvp.net/"

getData :: Text -> App (Response LByteString)
getData url = do
  Config {cSession = sess, cApi = (ApiConfig aKey)} <- ask
  let opts = defaults & param "api_key" .~ [aKey]
  getWith opts sess (unpack url) `catchHttp` handler
  where handler (StatusCodeException s _ _) = handlerError . decodeUtf8 $ s ^. statusMessage
        handler _ = handlerError "Network Error"
        handlerError msg = msg ++ " (" ++ url ++ ")"

getSummonerId :: App Integer
getSummonerId = do
  (name, region) <- (cName &&& toLower . cRegion) <$> ask
  let url = urlRoot region ++ "api/lol/" ++ region ++ "/v1.4/summoner/by-name/" ++ name
      nameKey = key . toLower . replace " " "" $ name
      getId = (^? responseBody . nameKey . key "id" . _Integer)
  getId <$> getData url >>= maybe (throw "Can't get summoner id") return

getCurrentGame :: Integer -> App GameInfo
getCurrentGame summonerId = do
  (region, platform) <- (toLower . cRegion &&& getPlatform) <$> ask
  let url = urlRoot region ++ "observer-mode/rest/consumer/getSpectatorGameInfo" ++ "/"
            ++ platform ++ txt summonerId
  participants <- getParticipants <$> getData url
  (divisions, champions) <- runConcurrently $ (,)
                            <$> Concurrently (getDivisions participants)
                            <*> Concurrently (getChampions participants)
  updateCache participants champions
  return $ map (map (\(c, n, d, _) -> (c, n, d)))
         $ groupAllOn (^. _4)
         $ zipWith3 (\(n, _, t, _) d c -> (c, n, d, t)) participants divisions champions
  where getParticipants = toListOf $ responseBody . key "participants" . values
                          . to ((,,,)
                                <$> (^?! key "summonerName" . _String)
                                <*> (^?! key "summonerId" . _Integer)
                                <*> (^?! key "teamId" . _Integer)
                                <*> (^?! key "championId" . _Integer))
        getDivisions = getSummonersDivision . map (^. _2)
        getChampions = mapConcurrently $ getChampionName . (^. _4)
        getPlatform = (++ "1/") . toUpper . take 3 . cRegion
        updateCache p c = modify (++ mapFromList (zip (map (txt . (^. _4)) p) c))

getSummonersDivision :: [Integer] -> App [Text]
getSummonersDivision summonerIds = do
  region <- toLower . cRegion <$> ask
  let url = urlRoot region ++ "api/lol/" ++ region ++ "/v2.5/league/by-summoner/"
            ++ sids ++ "/entry"
  r <- getData url
  return $ map (getRankedSolo5x5 . getDivision r) summonerIds
  where sids = intercalate "," . map txt $ summonerIds
        getRankedSolo5x5 = maybe "Unranked" (\(_, t, d) -> t ++ " " ++ d)
                           . headMay . filter ((== "RANKED_SOLO_5x5") . (^. _1))
        getDivision json sid = json ^.. responseBody . key (txt sid) . values
                               . to ((,,)
                                     <$> (^?! key "queue" . _String)
                                     <*> (^?! key "tier" . _String)
                                     <*> (^?! key "entries" . nth 0 . key "division"
                                          . _String))

getChampionName :: Integer -> App Text
getChampionName championId = lookup (txt championId) <$> get
                             >>= maybe (getName <$> getData url) return
  where url = urlRoot "global" ++ "api/lol/static-data/eune/v1.2/champion/" ++ txt championId
        getName = (^?! responseBody . key "name" . _String)

parseApiConfig :: ExceptT AppError IO ApiConfig
parseApiConfig = readFile "config.json" `catchAny` fileError
                 >>= maybe parseError return . decode
  where fileError = const $ throw "Cannot find config file"
        parseError = throw "Cannot parse config file"

parseArgs :: IO (Text, Text)
parseArgs = execParser $ info (helper <*> parser)
        (fullDesc
         <> progDesc "Get current game info for SUMMONER in REGION (na euw eune)"
         <> header "lolstats")
  where parser = (,)
                 <$> (pack <$> strArgument (metavar "REGION"))
                 <*> (pack <$> strArgument (metavar "SUMMONER"))

run :: (Text, Text) -> Session -> ExceptT AppError IO GameInfo
run (region, name) sess = do
  apiConfig <- parseApiConfig
  let cfg = Config apiConfig sess region name
  (result, cache) <- runApp app cfg =<< readCache
  writeCache cache
  return result
  where readCache = fromMaybe mempty . decode
                    <$> readFile "champions.json" `catchAny` const (return "")
        writeCache cache = writeFile "champions.json" (encode cache)
                           `catchAny` const (throw "Cannot write cache file (champions.json)")

app :: App GameInfo
app = getSummonerId >>= getCurrentGame

main :: IO ()
main = do
  args <- parseArgs
  withSession (either print (putStrLn . pack . ppShow) <=< runExceptT . run args)
