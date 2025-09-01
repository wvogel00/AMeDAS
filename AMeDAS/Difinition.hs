{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module AMeDAS.Difinition where
import qualified Data.HashMap.Strict as HM
import qualified Data.Text as T
import           Data.Text (Text)
import qualified Data.Vector as V
import Data.Aeson --(FromJSON(..), (.:?), (.:), withObject)

type Pos = (Double, Double) 
type ID = Int
type Temperature = Double
data Sampling = Sampling Start Stop Resolution
data Start = Start Year Month Day Hour Minute Second
data Stop = Stop Year Month Day Hour Minute Second
type Resolution = Minute -- アメダスデータ周期が10分
type Year   = Int
type Month  = Int
type Day    = Int
type Hour   = Int
type Minute = Int
type Second = Int
type Longtitude = Double
type Latitude = Double

temp35File = "data/temp35.dat"
temp30File = "data/temp30.dat"
temp25File = "data/temp25.dat"
temp0File = "data/temp0.dat"
tempAllFile = "data/tempall.dat"
othersFile = "data/others.dat"

defStart (y, m, d, h, mt, s) = Start y m d h mt s
defStop  (y, m, d, h, mt, s) = Start y m d h mt s

-- DB生成用 ------------
type StationTable = HM.HashMap Int Station

data Station = Station
  { stype  :: Text
  , elems  :: Text
  , lat    :: [Double]
  , lon    :: [Double]
  , alt    :: Double
  , kjName :: Text   -- 漢字の観測所名
  , knName :: Text   -- hurigana
  , enName :: Text   -- English
  }

instance Show Station where
  show s =   "type: " ++ (T.unpack $ stype s)
          ++ "\nelems: " ++ (T.unpack $ elems s)
          ++ "\n緯度: " ++ (show $ lat s)
          ++ "\n経度: " ++ (show $ lon s)
          ++ "\n高度: " ++ (show $ alt s)
          ++ "\n名称: " ++ (T.unpack $ kjName s) ++ "(" ++ (T.unpack $ knName s) ++ "," ++ (T.unpack $ enName s) ++ ")"

instance FromJSON Station where
  parseJSON = withObject "Station" $ \o -> Station
    <$> o .: "type"
    <*> o .: "elems"
    <*> o .: "lat"
    <*> o .: "lon"
    <*> o .: "alt"
    <*> o .: "kjName"
    <*> o .: "knName"
    <*> o .: "enName"

dmsToDeg [d,m] = d + m/60
------------------------------
-- AMeDAS --------------------
type WeatherMap = HM.HashMap Int Weather
data Measurement = Measurement {value :: Double, flag :: Maybe Double} deriving (Show, Eq)

instance FromJSON Measurement where
  parseJSON = withArray "Measurement" $ \arr -> do
    case V.toList arr of
      (x:y:_) -> Measurement <$> parseJSON x <*> parseJSON y
      (x:_)   -> Measurement <$> parseJSON x <*> pure Nothing
      _                      -> fail "Invalid Measurement format"


-- 各種気象データ
data Weather = Weather
  { temp             :: Maybe Measurement
  , humidity         :: Maybe Measurement
  , snow1h           :: Maybe Measurement
  , snow6h           :: Maybe Measurement
  , snow12h          :: Maybe Measurement
  , snow24h          :: Maybe Measurement
  , sun10m           :: Maybe Measurement
  , sun1h            :: Maybe Measurement
  , precipitation10m :: Maybe Measurement
  , precipitation1h  :: Maybe Measurement
  , precipitation3h  :: Maybe Measurement
  , precipitation24h :: Maybe Measurement
  , windDirection    :: Maybe Measurement
  , wind             :: Maybe Measurement
  } deriving (Show, Eq)

instance FromJSON Weather where
  parseJSON = withObject "Weather" $ \o -> Weather
    <$> o .:? "temp"
    <*> o .:? "humidity"
    <*> o .:? "snow1h"
    <*> o .:? "snow6h"
    <*> o .:? "snow12h"
    <*> o .:? "snow24h"
    <*> o .:? "sun10m"
    <*> o .:? "sun1h"
    <*> o .:? "precipitation10m"
    <*> o .:? "precipitation1h"
    <*> o .:? "precipitation3h"
    <*> o .:? "precipitation24h"
    <*> o .:? "windDirection"
    <*> o .:? "wind"

