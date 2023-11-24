module WeatherFetcher (getWeather) where

import Listener (ListenerMonad, speak, writeToMailBox)

-- import WeatherApi.WWOnline

mykey :: String
mykey = "top-secret"

mycity, mystate :: String
mycity = "Philadelphia"
mystate = "PA"

getWeather :: String -> String -> ListenerMonad ()
getWeather key zip = writeToMailBox "test"
