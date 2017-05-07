import Data.Char (toUpper)
import qualified Network.Socket as S
import qualified OpenSSL.Session as SSL
import qualified OpenSSL.X509 as X509
import OpenSSL (withOpenSSL)

import qualified OpenSSL.RSA as RSA
import qualified OpenSSL.EVP.PKey as PKey

import Control.Exception (try, SomeException)
import Control.Monad.Trans (lift)
import qualified Control.Monad.Trans.Either as ET

import qualified Numeric as N
import qualified Data.Hash.MD5 as MD5


--serverHost = "01.mqtt.syncleo-iot.com"
--serverPort = 8883


type Host = String
type Port = Int
type SrvKey = String


data ProcErrors = ConnectFail | RetrieveFail | PKObtainFail deriving (Eq, Show)


getServerCrt :: Host -> Port -> IO (Either ProcErrors X509.X509)
getServerCrt h p = do
    res <- try $ getSrvCrtProc h p :: IO (Either SomeException (Maybe X509.X509))
    case res of
        Left except      -> return $ Left ConnectFail
        Right (Just crt) -> return $ Right crt
        otherwise        -> return $ Left RetrieveFail
    where
        getSrvCrtProc :: Host -> Port -> IO (Maybe X509.X509)
        getSrvCrtProc h p = do
        -- connect
        addrInfo <- S.getAddrInfo Nothing (Just h) (Just $ show p)
        let serverAddr = head addrInfo
        sock <- S.socket (S.addrFamily serverAddr) S.Stream S.defaultProtocol
        S.connect sock (S.addrAddress serverAddr)
        -- wrap socket
        ctx <- SSL.context
        wrappedSSLSocket <- SSL.connection ctx sock
        _ <- SSL.connect wrappedSSLSocket    
        SSL.getPeerCertificate wrappedSSLSocket


getPubKey :: X509.X509 -> IO (Either ProcErrors RSA.RSAPubKey)
getPubKey crt = do
    pkobj <- X509.getPublicKey crt
    case PKey.toPublicKey pkobj of
        (Just pk) -> return $ Right pk
        Nothing   -> return $ Left PKObtainFail


-- compatible with standard signature calculation using openssl:
-- openssl s_client -connect 127.0.0.1:11011 | openssl x509 -noout -modulus | openssl md5
keySignature :: RSA.RSAPubKey -> String
keySignature = getMD5Sum . prepend . map toUpper . int2hex . RSA.rsaN
    where
        int2hex :: Integer -> String
        int2hex n = N.showHex n "\n"
        prepend :: String -> String
        prepend s = "Modulus=" ++ s
        getMD5Sum :: String -> String
        getMD5Sum = MD5.md5s . MD5.Str


getServerKey :: Host -> Port -> IO (Either ProcErrors RSA.RSAPubKey)
getServerKey h p = ET.runEitherT $ do
    crtRes <- lift $ getServerCrt h p
    crt <- ET.hoistEither crtRes
    pkRes <- lift $ getPubKey crt
    pk <- ET.hoistEither pkRes
    return pk


checkServerKey :: Host -> Port -> SrvKey -> IO ()
checkServerKey h p refKey = do
    keyRes <- getServerKey h p
    case keyRes of
        -- TODO set values in prometheus instead of printing
        Left err -> putStrLn $ "key check error: " ++ errToMetric err
        Right key -> if refKey == keySignature key
                        then putStrLn "ok, key matches!"
                        else putStrLn "failure, key doesn't match"


-- TODO use prometheus gauge values instead of strings: i.e Int
errToMetric :: ProcErrors -> String
errToMetric ConnectFail  = "server connection error"
errToMetric RetrieveFail = "certificate retrieval error"
errToMetric PKObtainFail = "public key retrieval error"


serverHost = "127.0.0.1"
serverPort = 11011


main = withOpenSSL $ do
    -- TODO add cert expiration time
    {-
    pkRes <- getServerKey serverHost serverPort
    case pkRes of
        Left exc -> putStrLn $ "certificate verification failed: " ++ show exc
        Right pk -> putStrLn $ "key signature: " ++ keySignature pk
    -}
    let sampleKey = "790827182fb3a1fd395c69976778d34e"
    checkServerKey serverHost serverPort sampleKey


