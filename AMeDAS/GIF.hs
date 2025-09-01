{-# LANGUAGE OverloadedStrings #-}

module AMeDAS.GIF where

import System.Process (callCommand)
import AMeDAS.Difinition
import System.Directory (removeFile)
import AMeDAS.Difinition
import AMeDAS.StationTable
import AMeDAS.AMeDAS
import AMeDAS.Snapshot

gifFile = "output.gif"

addHour :: Date -> Hour -> Date
addHour (Date y m d h mt s) dh = Date y m (d+div dh 24) (mod dh 24) mt s


seqPlotFile k = "data/" ++ f 4 (show k) ++ ".png" where
  f n str = replicate (n-length str) '0' ++ str

genInOutFilepath :: Date -> [Hour] -> [(FilePath, FilePath)]
genInOutFilepath s hs = zip (map (getJSONpath . addHour s) hs) (map seqPlotFile [1 ..])

drawGIF :: Start -> [Hour] -> IO ()
drawGIF s dhs = do
  tb <- getStationMap
  let (ins,outs) = unzip $ genInOutFilepath s dhs
  wms <- mapM getAMeDASData  ins
  mapM_ (\(file,wm) -> drawTempPNG file wm)  $ zip outs wms

  callCommand $ "convert -dispose Background -delay 20 -loop 0 data/*.png " ++ gifFile
  putStrLn $ gifFile ++ " が生成されました。"
  callCommand "rm data/*.png"
