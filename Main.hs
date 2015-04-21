{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TypeFamilies               #-}

module Main where

import           ClassyPrelude
import           Control.Concurrent.Async.Lifted
import           Control.Lens
import           Control.Monad.Base
import           Control.Monad.Error
import           Control.Monad.Trans.Control
import           Data.Aeson                      (FromJSON, decode)
import           Data.Aeson.Lens
import           Data.Text                       (replace)
import           Network.HTTP.Client             (HttpException (StatusCodeException))
import           Network.Wreq                    hiding (getWith)
import           Network.Wreq.Session
import           Text.Show.Pretty

newtype App a = App (ReaderT Config (ErrorT AppError IO) a)
              deriving (Functor, Applicative, Monad, MonadIO, MonadReader Config
                       , MonadError AppError, MonadBase IO)

instance MonadBaseControl IO App where
  type StM App a = Either AppError a
  liftBaseWith f = App $ liftBaseWith $ \run -> f (run . unApp)
    where unApp (App a) = a
  restoreM = App . restoreM

newtype AppError = AppError Text

instance Show AppError where
  show (AppError e) = "Error: " ++ unpack e

instance Error AppError where
  strMsg = AppError . pack

data ApiConfig = ApiConfig { apiKey :: Text } deriving (Show, Generic)
instance FromJSON ApiConfig

data Config = Config { cApi     :: ApiConfig
                     , cSession :: Session
                     , cRegion  :: Text
                     , cName    :: Text
                     } deriving (Show)

txt :: Show a => a -> Text
txt = pack . show

throw :: MonadError AppError m => Text -> m a
throw = throwError . AppError

runApp :: Show a => App a -> Config -> IO (Either AppError a)
runApp (App a) = runErrorT . runReaderT a

catchHttp :: IO a -> (HttpException -> Text) -> App a
catchHttp action handler = liftIO (catch (Right <$> action) (return . Left . handler))
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

getCurrentGame :: Integer -> App [[(Text, Text, Text)]]
getCurrentGame summonerId = do
  (region, platform) <- (toLower . cRegion &&& getPlatform) <$> ask
  let url = urlRoot region ++ "observer-mode/rest/consumer/getSpectatorGameInfo" ++ "/"
            ++ platform ++ (txt summonerId)
  participants <- getParticipants <$> getData url
  (divisions, champions) <- runConcurrently $ (,)
                            <$> Concurrently (getDivisions participants)
                            <*> Concurrently (getChampions participants)
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
getChampionName championId = getName <$> getData url
  where url = urlRoot "global" ++ "api/lol/static-data/eune/v1.2/champion/" ++ txt championId
        getName = (^?! responseBody . key "name" . _String)

app :: App ()
app = getSummonerId >>= getCurrentGame >>= putStrLn . pack . ppShow

parseApiConfig :: ErrorT AppError IO ApiConfig
parseApiConfig = do
  json <- readFile "config.json" `catchAny` fileError
  let cfg = decode json :: Maybe ApiConfig
  maybe parseError return cfg
  where fileError = const $ throw "Cannot find config file"
        parseError = throw "Cannot parse config file"

parseArgs :: ErrorT AppError IO (Text, Text)
parseArgs = do
  region <- (headMay <$> getArgs) `orThrow` "Need summoner region as first argument"
  name <- (headMay . tailEx <$> getArgs) `orThrow` "Need summoner name as second argument"
  return (region, name)
  where orThrow action msg = action >>= maybe (throw msg) return

main :: IO ()
main = withSession $ \sess -> do
  let cfg = do
        apiConfig <- parseApiConfig
        (region, name) <- parseArgs
        return $ Config apiConfig sess region name
  runErrorT cfg >>= either print (runApp app >=> either print return)
