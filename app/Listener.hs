{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Listener where

import Numeric ( showFFloat )
import Sound.VAD.WebRTC as Vad
    ( VAD )

import Control.Monad.IO.Class
import Control.Monad.ST ( RealWorld )
import Control.Monad.Reader (ReaderT)
import Control.Monad.Except (ExceptT)
import Control.Monad
import Control.Monad.State
    ( evalStateT, StateT(runStateT), MonadState(put, get), lift )


import Data.Maybe ( fromMaybe, isJust, isNothing )
import Data.Time.Clock ( UTCTime )
import System.Process

data AppError = ReadAudioError | SystemError

data EnvConfig = EnvConfig { localpath :: FilePath }

data ListenerState = ListenerState {
                                path :: FilePath,
                                startTime :: UTCTime,
                                timeOffset :: Double,
                                voiceStartTime :: Maybe Double,
                                voiceEndTime :: Maybe Double,
                                segmentDuration :: Double,
                                limit :: Double,
                                vad :: Vad.VAD RealWorld,
                                count :: Int
                              }

newtype ListenerMonad a = ListenerMonad (StateT ListenerState IO a)
    deriving (Functor, Applicative, Monad, MonadIO )


runListenerMonad :: ListenerMonad a -> ListenerState -> IO a
runListenerMonad  (ListenerMonad stateAction) listenerState = 
    evalStateT stateAction listenerState 
 
instance MonadState ListenerState ListenerMonad where
  get = ListenerMonad get
  put = ListenerMonad . put

initialState currentTime vad = ListenerState { 
  startTime = currentTime,
  path = "in.wav", 
  timeOffset=0.0, 
  segmentDuration=2.0, 
  limit=30.0, 
  vad=vad, 
  voiceStartTime = Nothing,
  voiceEndTime = Nothing,
  count = 0
}

getStartEnd :: Maybe Double -> Maybe Double -> Maybe (Double, Double)
getStartEnd start end = do s <- start
                           e <- end
                           return (s,e)

calcDuration :: ListenerState -> Maybe Double
calcDuration listener = let se = getStartEnd (voiceEndTime listener) (voiceStartTime listener)
                            sub = uncurry (-)
                         in fmap sub se
 
speak :: a -> ListenerMonad a
speak v = return v 

debugPrint :: ListenerState -> IO ()
debugPrint listener = do print "------------------------------"
                         print "count:"
                         print (count listener)
                         print "time offset:"
                         print (timeOffset listener)
                         print "duration:"
                         print (segmentDuration listener)
                         print "voiceStart:"
                         print (voiceStartTime listener)
                         print "voiceEnd:"
                         print (voiceEndTime listener)
                         print "duration:"
                         print $ calcDuration listener

  


