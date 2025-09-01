{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module AMeDAS.StationTable (getStationMap) where

import Network.HTTP.Conduit (simpleHttp)
import qualified Data.HashMap.Strict as HM
import qualified Data.Text as T
import           Data.Text (Text)
import           Data.Maybe (fromJust, mapMaybe)
import           Data.Aeson (eitherDecode, FromJSON(..), (.:?), (.:), withObject)
import AMeDAS.Difinition


parseAmeMaster jsonStr = do
  let result = eitherDecode jsonStr :: Either String StationTable
  case result of
    Left err -> return HM.empty 
    Right infos -> return infos

getStationMap :: IO StationTable
getStationMap = do
  jsonBS <- simpleHttp "https://www.jma.go.jp/bosai/amedas/const/amedastable.json"
  return =<< parseAmeMaster jsonBS

