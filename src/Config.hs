{-# LANGUAGE OverloadedStrings #-}

module Config where

import qualified Data.ByteString.Char8 as BS
import qualified Data.Yaml as Y
import Data.Yaml (FromJSON(..), (.:))

import Control.Monad.Trans.Maybe (MaybeT(..), runMaybeT)
import System.Environment (getArgs)


data Config = Config { general      :: GeneralOpts,
                       crtCheckOpts :: [CrtOpts],
                       keyCheckOpts :: [KeyOpts] } deriving (Eq, Show)

instance FromJSON Config where
    parseJSON (Y.Object m) = Config <$>
        m .: "general"      <*>
        m .: "certificates" <*>
        m .: "keys"
    parseJSON x = fail ("not an object: " ++ show x)


data GeneralOpts = GeneralOpts { promPort      :: Int,
                                 metricPath    :: String,
                                 checkInterval :: Int } deriving (Eq, Show)

instance FromJSON GeneralOpts where
    parseJSON (Y.Object m) = GeneralOpts <$>
        m .: "prometheus_port" <*>
        m .: "metric_path"     <*>
        m .: "check_interval"
    parseJSON x = fail ("not an object: " ++ show x)


data CrtOpts = CrtOpts { crtCheckHost :: String,
                         crtCheckPort :: Int } deriving (Eq, Show)

instance FromJSON CrtOpts where
    parseJSON (Y.Object m) = CrtOpts <$>
        m .: "host" <*>
        m .: "port"
    parseJSON x = fail ("not an object: " ++ show x)


data KeyOpts = KeyOpts { keyCheckHost   :: String,
                         keyCheckPort   :: Int,
                         keyCheckRefKey :: String } deriving (Eq, Show)

instance FromJSON KeyOpts where
    parseJSON (Y.Object m) = KeyOpts <$>
        m .: "host" <*>
        m .: "port" <*>
        m .: "key"
    parseJSON x = fail ("not an object: " ++ show x)


loadConfig :: FilePath -> IO (Maybe Config)
loadConfig fp = do
    content <- BS.readFile fp
    let parsed = Y.decode content :: Maybe Config
    return parsed


getConfFileName :: IO (Maybe FilePath)
getConfFileName = do
    args <- getArgs
    case length args of
        1 -> return $ Just (head args)
        _ -> return Nothing


readConfigFile :: IO (Maybe Config)
readConfigFile = runMaybeT $ do
    fname <- MaybeT getConfFileName
    MaybeT $ loadConfig fname
