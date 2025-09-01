module AMeDAS.StationTable (getStationMap) where

import Network.HTTP.Conduit (simpleHttp)
import qualified Data.HashMap.Strict as HM
import Data.Aeson 
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

