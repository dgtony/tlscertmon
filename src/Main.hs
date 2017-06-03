{-# LANGUAGE OverloadedStrings #-}

import TLSProcessing
import Config

import OpenSSL (withOpenSSL)
import Data.Text (pack)
import System.Environment (getProgName)

import System.Metrics.Prometheus.Concurrent.Http (serveHttpTextMetricsT)
import System.Metrics.Prometheus.MetricId (fromList)
import qualified System.Metrics.Prometheus.Concurrent.RegistryT as PRT
import qualified System.Metrics.Prometheus.Metric.Gauge as G

import Control.Concurrent (forkIO, threadDelay)
import Control.Monad.IO.Class (liftIO, MonadIO)
import Control.Monad (forever, foldM, forM, forM_)


-- defines
crtMetricName = "crt_mon_exp"
crtMetricLabel = "host"
keyMetricName = "crt_mon_key_valid"
keyMetricLabel = "host"


registerMetricCrt :: MonadIO m => CrtOpts -> PRT.RegistryT m G.Gauge
registerMetricCrt mOpts = do
    let metricLabel = pack $ crtCheckHost mOpts ++ ":" ++ show (crtCheckPort mOpts)
    PRT.registerGauge crtMetricName (fromList [(crtMetricLabel, metricLabel)])


registerMetricKey :: MonadIO m => KeyOpts -> PRT.RegistryT m G.Gauge
registerMetricKey mOpts = do
    let metricLabel = pack $ keyCheckHost mOpts ++ ":" ++ show (keyCheckPort mOpts)
    PRT.registerGauge keyMetricName (fromList [(keyMetricLabel, metricLabel)])


runCheckCrt
    :: G.Gauge
    -> Host
    -> Port
    -> Int
    -> IO a
runCheckCrt metric host port interval = forever $ do
    res <- checkServerCrt host port
    G.set res metric
    threadDelay interval


runCheckKey
    :: G.Gauge
    -> Host
    -> Port
    -> SrvKey
    -> Int
    -> IO a
runCheckKey metric host port refKey interval = forever $ do
    res <- checkServerKey host port refKey
    G.set res metric
    threadDelay interval


runPrometheus :: Config -> IO ()
runPrometheus config = PRT.runRegistryT $ do
    let crtOpts = crtCheckOpts config
        keyOpts = keyCheckOpts config
        period  = repeat $ 1000000 * (checkInterval . general $ config)
        path    = metricPath . general $ config

    -- register all metrics
    crtMetrics <- forM crtOpts registerMetricCrt
    keyMetrics <- forM keyOpts registerMetricKey

    -- run check procedures
    liftIO $ forM_ (zip3 crtMetrics crtOpts period) (forkIO . transformArgCrt runCheckCrt)
    liftIO $ forM_ (zip3 keyMetrics keyOpts period) (forkIO . transformArgKey runCheckKey)

    -- run prometheus server
    serveHttpTextMetricsT (promPort . general $ config) [pack path]

    where
        transformArgCrt f (m, co, i) = f m (crtCheckHost co) (crtCheckPort co) i
        transformArgKey f (m, co, i) = f m (keyCheckHost co) (keyCheckPort co) (keyCheckRefKey co) i


printUsage :: String -> IO ()
printUsage monitorName = do
    putStrLn $ "Usage: ./" ++ monitorName ++ " <config_file_path>"


printStats :: Config -> IO ()
printStats config = do
    let numCrts = length $ crtCheckOpts config
        numKeys = length $ keyCheckOpts config
    putStrLn "monitoring started..."
    putStrLn $ "certificates: " ++ show numCrts
    putStrLn $ "public keys:  " ++ show numKeys



main = withOpenSSL $ do
    monitorName <- getProgName
    confFile <- readConfigFile
    case confFile of
        Just config -> printStats config >> runPrometheus config
        Nothing     -> printUsage monitorName
