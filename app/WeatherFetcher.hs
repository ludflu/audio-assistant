module WeatherFetcher (getWeather) where

import Listener ( ListenerMonad, speak ) 


--import WeatherApi.WWOnline

mykey :: String
mykey   = "top-secret"

mycity, mystate :: String
mycity  = "Philadelphia"
mystate = "PA"

getWeather :: String -> String -> ListenerMonad String
getWeather key zip = return "test"

