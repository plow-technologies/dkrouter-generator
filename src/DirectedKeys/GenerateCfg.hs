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
  let delta = ceiling $ (toRational . length $ aList) / (toRational . length $ bList) -- delta = quot ((length aList)) ((length bList))
      groupedOnDelta = revLast . reverse $ groupUp delta aList
  in createBoundsTuples  ((map (!! 0) groupedOnDelta)) bList

createBoundsTuples :: [a] -> [b] -> [(a,b)]
createBoundsTuples al bl = zipWith (\a b -> (a,b)) al bl

revLast :: [[a]] -> [[a]]
revLast (x:[]) = [reverse x]
revLast (x:xs) = x:(revLast xs)

mConf :: MongoDBConf
mConf = MongoDBConf "127.0.0.1" "onping_production" 27017