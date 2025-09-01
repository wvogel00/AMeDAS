{-# LANGUAGE OverloadedStrings #-}

module Gnuplot where

import System.Process (callCommand)
import System.IO (writeFile)
import AMeDAS.Difinition
import System.Directory (removeFile)
import AMeDAS.Difinition
import AMeDAS.StationTable
import AMeDAS

mapPath = "map/world_10m.txt"
scriptFile = "tempplot.gp"

makeFile filepath ts = writeFile filepath . unlines $ map (format.snd) ts where
  format (x,y) = show x ++ " " ++ show y

drawTempPNG :: FilePath  -> IO ()
drawTempPNG path = do
    tb <- getStationMap
    wm <- getAMeDASData
    let ts = makeTempList tb wm
    print ts
    let above35 = filter ((>=35).fst) ts
    let above30 = filter ((\v -> v>=30 && 35>v).fst) ts
    let above25 = filter ((\v -> v>=25 && 30>v).fst) ts
    let others = filter ((\v -> v<25).fst) ts
    --mapM_ removeFile [temp35File, temp30File]
    makeFile temp35File above35
    makeFile temp30File above30
    makeFile temp25File above25
    makeFile othersFile others
    let script = unlines [ "set terminal pngcairo size 1200,900"
                         , "set output '" ++ path ++ "'"
                         , "set xrange [125:150]"
                         , "set yrange [29:49]"
                         --, "set nokey"
                         , "set style line 1 lt 1 lw 1 lc rgb '#d3d3d3'"
                         , "plot '" ++  mapPath ++ "' with filledcurves ls 1, \\"
                         , "\"\" with lines ls 1, \\"  -- 地図
                         , "'" ++othersFile++"' using 1:2 with points pt 5 ps 1 lc rgb 'green' title '<25', \\"  -- >=30
                         , "'" ++temp25File++"' using 1:2 with points pt 5 ps 1 lc rgb 'yellow' title '>25', \\"  -- >=30
                         , "'" ++temp30File++"' using 1:2 with points pt 5 ps 1 lc rgb 'orange' title '>30', \\"  -- >=30
                         , "'"++temp35File++ "' using 1:2 with points pt 5 ps 1 lc rgb 'red' title '>35'"   -- >=35
                         ]

    writeFile scriptFile script
    callCommand $ "gnuplot " ++ scriptFile
    putStrLn $ path ++ " が生成されました。"
    removeFile scriptFile
    mapM_ removeFile [temp35File, temp30File, temp25File, othersFile]

