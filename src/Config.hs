{-# LANGUAGE OverloadedStrings #-}

module Config where

import qualified Data.ByteString.Char8 as BS
import qualified Data.Yaml as Y
import Data.Yaml (FromJSON(..), (.:))

import qualified Control.Monad.Trans.Maybe as MBT
import Control.Monad.Trans (liftIO)
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
                                 checkInterval :: Int } deriving (Eq, Show)

instance FromJSON GeneralOpts where
    parseJSON (Y.Object m) = GeneralOpts <$>
        m .: "prometheus_port" <*>
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


readConfig :: FilePath -> IO (Maybe Config)
readConfig fp = do
    content <- BS.readFile fp
    let parsed = Y.decode content :: Maybe Config
    return parsed


getConfFileName :: IO (Maybe FilePath)
getConfFileName = do
    args <- getArgs
    case length args of
        1 -> return $ Just (head args)
        _ -> return Nothing


getConfigFile :: IO (Maybe Config)
getConfigFile = MBT.runMaybeT $ do
    f <- MBT.MaybeT $ liftIO getConfFileName
    MBT.MaybeT $ liftIO $ readConfig f

