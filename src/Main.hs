{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

import           Control.Applicative
import qualified Data.Aeson               as A
import qualified Data.ByteString          as BS
import qualified Data.ByteString.Lazy     as BSL
import           Data.Maybe
import qualified Data.Text                as T
import qualified Data.Traversable         as T
import           Data.Yaml
import           Database.Persist
import           Database.Persist.Types
import           DirectedKeys.GenerateCfg
import           GHC.Generics
import           Persist.Mongo.Settings
import           Prelude

main :: IO ()
main = do
  dbConf <- readDBConf "config.yml"
  genConf <- readKeyGen "config.yml"
  let hosts = (hostList genConf)
  eBoundList <- T.sequence $ (\conf -> createAndMatchKeys conf (fromJust . onpingTagCombinedPid . entityVal) hosts) <$> dbConf
  case eBoundList of
    Left _ -> putStrLn "Error reading config file"
    Right boundList -> BSL.putStrLn . A.encode $ listToOutput hosts boundList

readKeyGen :: FilePath -> IO KeyGenConfig
readKeyGen fp = do
  contents <- BS.readFile fp
  case (decodeEither contents) of
    Left _ -> fail "Unable to parse KeyGenConfig"
    Right kgcfg -> return kgcfg


listToOutput :: [String] -> [(Int,Int)] -> [KeyGenOutput]
listToOutput sList bList = zipWith (\host (upper,lower) -> KeyGenOutput host upper lower) sList bList

data KeyGenConfig = KeyGenConfig {
  hostList :: [String]
} deriving (Eq, Show, Generic)

instance FromJSON KeyGenConfig where

data KeyGenOutput = KeyGenOutput {
  keyGenHost       :: String
, keyGenUpperBound :: Int
, keyGenLowerBound :: Int
} deriving (Eq, Show)


instance ToJSON KeyGenOutput where
  toJSON (KeyGenOutput {..}) = object ["host" .= keyGenHost
                                      ,"upperBound" .= keyGenUpperBound
                                      ,"lowerBound" .= keyGenLowerBound]
