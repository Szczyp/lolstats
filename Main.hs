{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TupleSections              #-}
{-# LANGUAGE TypeFamilies               #-}

module Main where

import           ClassyPrelude
import           Control.Concurrent.Async.Lifted
import           Control.Lens
import           Control.Monad.Base
import           Control.Monad.Error
import           Control.Monad.Trans.Control
import           Data.Aeson                      (FromJSON, ToJSON, decode)
import           Data.Aeson.Lens
import           Data.Text                       (replace)
import           Network.HTTP.Client             (HttpException (StatusCodeException))
import           Network.Wreq                    hiding (getWith)
import           Network.Wreq.Session
import           Text.Show.Pretty

newtype App a = App { unApp :: ReaderT (Session, Config) (ErrorT AppError IO) a }
              deriving (Functor, Applicative, Monad, MonadIO, MonadReader (Session, Config)
                       , MonadError AppError, MonadBase IO)

instance MonadBaseControl IO App where
  type StM App a = Either AppError a
  liftBaseWith f = App $ liftBaseWith $ \run -> f (run . unApp)
  restoreM = App . restoreM

newtype AppError = AppError Text

instance Show AppError where
  show (AppError e) = "Error: " ++ unpack e

instance Error AppError where
  strMsg = AppError . pack

data Config = Config { apiUrl :: Text
                     , apiKey :: Text
                     }
            deriving (Show, Generic)

instance FromJSON Config
instance ToJSON Config

txt :: Show a => a -> Text
txt = pack . show

throw :: MonadError AppError m => Text -> m a
throw = throwError . AppError

runApp :: Show a => App a -> (Session, Config) -> IO (Either AppError a)
runApp (App a) = runErrorT . runReaderT a

catchHttp :: IO a -> (HttpException -> Text) -> App a
catchHttp action handler = liftIO (catch (Right <$> action) (return . Left . handler))
                           >>= either throw return

getData :: Text -> App (Response LByteString)
getData url = do
  (sess, (Config aUrl aKey)) <- ask
  let opts = defaults & param "api_key" .~ [aKey]
  getWith opts sess (unpack $ aUrl ++ url) `catchHttp` handler
  where handler (StatusCodeException s _ _) = decodeUtf8 $ s ^. statusMessage
        handler _ = "Network Error"

getSummonerId :: Text -> App Integer
getSummonerId name = getId <$> getData url
                     >>= maybe (throw "Can't get summoner id") return
  where url = "api/lol/eune/v1.4/summoner/by-name/" ++ name
        nameKey = key . toLower . replace " " "" $ name
        getId = (^? responseBody . nameKey . key "id" . _Integer)

getCurrentGame :: Text -> Integer -> App [[(Text, Text, Text)]]
getCurrentGame region summonerId = do
  participants <- getParticipants <$> getData url
  (divisions, champions) <- runConcurrently $ (,)
                            <$> Concurrently (getDivisions participants)
                            <*> Concurrently (getChampions participants)
  return $ map (map (\(c, n, d, _) -> (c, n, d)))
         $ groupAllOn (\(_, _, _, t) -> t)
         $ zipWith3 (\(n, _, t, _) d c -> (c, n, d, t)) participants divisions champions
  where url = "observer-mode/rest/consumer/getSpectatorGameInfo"
              ++ "/" ++ region ++ "/" ++ (txt summonerId)
        getParticipants = (^.. responseBody . key "participants" . values
                              . to (\o -> ( o ^?! key "summonerName" . _String
                                          , o ^?! key "summonerId" . _Integer
                                          , o ^?! key "teamId" . _Integer
                                          , o ^?! key "championId" . _Integer)))
        getDivisions = getSummonersDivision . map (\(_, s, _, _) -> s)
        getChampions = mapConcurrently $ getChampionName . (\(_, _, _, c) -> c)

getSummonersDivision :: [Integer] -> App [Text]
getSummonersDivision summonerIds = do
  r <- getData url
  return $ map (getRankedSolo5x5 . getDivision r) summonerIds
  where sids = intercalate "," . map txt $ summonerIds
        url = "api/lol/eune/v2.5/league/by-summoner/" ++ sids ++ "/entry"
        getRankedSolo5x5 = maybe "Unranked" (\(_, t, d) -> t ++ " " ++ d)
                           . headMay . filter (\(q, _, _) -> q == "RANKED_SOLO_5x5")
        getDivision json sid = (json ^.. responseBody . key (txt sid) . values
                                . to (\o -> ( o ^?! key "queue" . _String
                                            , o ^?! key "tier" . _String
                                            , o ^?! key "entries" . nth 0 . key "division"
                                              . _String)))

getChampionName :: Integer -> App Text
getChampionName championId = getName <$> getData url
  where url = "api/lol/static-data/eune/v1.2/champion/" ++ txt championId
        getName = (^?! responseBody . key "name" . _String)

app :: App ()
app = do
  name <- headMay <$> getArgs >>= maybe (throw "Need summoner name as argument") return
  getSummonerId name >>= getCurrentGame "EUN1" >>= putStrLn . pack . ppShow

parseConfig :: ErrorT AppError IO Config
parseConfig = do
  json <- readFile "config.json" `catchAny` fileError
  let cfg = decode json :: Maybe Config
  maybe parseError return cfg
  where fileError = const $ throw "Cannot find config file"
        parseError = throw "Cannot parse config file"

main :: IO ()
main = withSession $ \sess ->
  runErrorT parseConfig >>= either print ((runApp app >=> either print return) . (sess,))
