module WeatherFetcher (getWeather) where

import Listener ( ListenerMonad, speak ) 


{-# LANGUAGE RecordWildCards #-}

-- import Web.Weather

mykey :: String
mykey   = "top-secret"

mycity, mystate :: String
mycity  = "Philadelphia"
mystate = "PA"

getWeather :: String -> String -> ListenerMonad String
getWeather key zip = return "test"
-- getWeather = do
--   resp <- getConditions mykey mycity mystate
--   case resp of
--    Nothing -> return  "No data for that city/state"
--    Just (Observation{..}) -> return obsWeather
    --  putStrLn $ "Observation time: " ++ obsTime
    --  putStrLn $ "Weather conditions: " ++ obsWeather
    --  putStrLn $ "Temp: " ++ show obsTemp
    --  putStrLn $ "Rel humidity: " ++ show obsRelHumidity
    --  putStrLn $ "Wind: " ++ obsWind
    --  putStrLn $ "Feels like: " ++ obsFeelsLike