{-# LANGUAGE OverloadedStrings #-}
module AMeDAS where

import Data.Aeson
import Network.HTTP.Simple
import qualified Data.ByteString.Lazy as BL
import qualified Data.HashMap.Strict as HM
import qualified Data.Text as T
import           Data.Text (Text)
import qualified Data.Text.IO as TIO
import Data.List (find)
import Data.List.Split (splitWhen)
import Data.Maybe
import Data.Time
import Data.Time.Format (formatTime, defaultTimeLocale)
import AMeDAS.Difinition
import AMeDAS.StationTable
import Debug.Trace (trace)


getLatestFileName = do
  now <- getCurrentTime
  tz <- getCurrentTimeZone
  let localNow = utcToLocalTime tz now
  let mm = latestMin $ formatTime defaultTimeLocale "%M" localNow
  let timestamp = formatTime defaultTimeLocale "%Y%m%d%H" localNow ++ mm ++ "00.json"
  let timeStr   = formatTime defaultTimeLocale "%Y/%m/%d %H:" localNow ++ mm ++ ":00"
  let s  = "00"   
  putStrLn timeStr
  return timestamp
  where
    latestMin :: String -> String
    latestMin m = let v = take 2 . show $ (div (read m) 10) * 10 - 10 in if length v==1 then '0':v else v

url = "https://www.jma.go.jp/bosai/amedas/data/map/"

getTempText :: Weather -> Text
getTempText w  = T.concat [T.pack . show $ value . fromJust $ temp w, " [℃]"]

getPos :: ID -> StationTable -> (Longtitude, Latitude)
getPos key tb = let v = fromJust $ HM.lookup key tb in (dmsToDeg $ lon v, dmsToDeg $ lat v)

getName :: ID -> StationTable -> Text
getName key tb = let v = fromJust $ HM.lookup key tb in kjName v

getAMeDASData :: IO WeatherMap
getAMeDASData = do
    latestFileName <- (++) url <$> getLatestFileName
    res <- httpLBS (parseRequest_ latestFileName)
    stationDB <- getStationMap
    let jsonFile = getResponseBody res
    let tb = eitherDecode jsonFile :: Either String WeatherMap
    case tb of
      Left err -> putStrLn "parse error" >> return HM.empty
      Right xs -> return xs

makeTempList :: StationTable -> WeatherMap -> [(Temperature, Pos)]
makeTempList tb wm = let ws = HM.toList wm in map g . filter available $ map f ws where
  f = \(key, w) -> (key, temp w)
  available (_,tmp) = case tmp of
    Nothing -> False
    Just v -> not $ isNaN (value v)
  g (key, Just v) = (value v, getPos key tb)

main :: IO ()
main = do
    result <- getAMeDASData
    if HM.toList result == []
      then putStrLn "最新データ到着待ち"
      else mapM_ print $ take 10 (HM.toList result)
    print "length:"
    print $ length (HM.toList result)
    print ""
