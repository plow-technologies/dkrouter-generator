{-# LANGUAGE TypeFamilies, RankNTypes, FlexibleContexts, OverloadedStrings #-}
module DirectedKeys.GenerateCfg where 

-- import DirectedKeys
import DirectedKeys.GenerateCfg.Internal
import Control.Applicative
import Data.Aeson
import Data.List
import Database.Persist.Class
import Database.Persist.Types
import Persist.Mongo.Settings
import Control.Monad.IO.Class
import Control.Monad.Trans.Control
import Database.Persist.MongoDB
import qualified Data.Text as T

createAndMatchKeys :: (MonadBaseControl IO m, PersistEntity val, Ord a,
                        MonadIO m, PersistEntityBackend val ~ MongoBackend) =>
                          MongoDBConf -> (Entity val -> a) -> [b] -> m [(a, b)]
createAndMatchKeys mongoConf collToKey destList = do
  collectionList <- runDBConf mongoConf $ selectList [] []
  let keyList = sort $ collToKey <$> collectionList
  return $ matchKeys keyList destList

getAllkeysTest :: (MonadBaseControl IO m, PersistEntity val, Ord a,
                        MonadIO m, PersistEntityBackend val ~ MongoBackend) =>
                          MongoDBConf -> (Entity val -> a) -> m [a]
getAllkeysTest mongoConf keyFcn = do
  collectionList <- runDBConf mongoConf $ selectList [] []
  return $ (keyFcn <$> collectionList) 


matchKeys :: [a] -> [b] -> [(a,b)]
matchKeys aList bList =
  let delta = quot (length aList) ((length bList) - 1)
      groupedOnDelta = reverse $ groupUp delta aList
      lastElem = last aList
  in createBoundsTuples  ((map (!! 0) groupedOnDelta) ++ [lastElem]) bList

createBoundsTuples :: [a] -> [b] -> [(a,b)]
createBoundsTuples al bl = zipWith (\a b -> (a,b)) al bl

mConf :: MongoDBConf
mConf = MongoDBConf "127.0.0.1" "onping_production" 27017