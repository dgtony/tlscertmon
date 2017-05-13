
import OpenSSL (withOpenSSL)

import TLSProcessing

import Config


--serverHost = "01.mqtt.syncleo-iot.com"
--serverPort = 8883



serverHost = "127.0.0.1"
serverPort = 11011


main = withOpenSSL $ do
    confFile <- getConfigFile
    case confFile of
        Just conf -> putStrLn $ "get config: " ++ show conf
        Nothing -> putStrLn "cannot read config file"
        --Nothing   -> fail "Usage: ./monitor <config_file_path>"


    let sampleKey = "790827182fb3a1fd395c69976778d34e"
    checkServerKey serverHost serverPort sampleKey
    checkServerCrt serverHost serverPort

