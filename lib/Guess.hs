{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Guess where

import Control.Concurrent (threadDelay)
import Control.Monad.State (liftIO)
import Data.Char (isNumber, toLower)
import Listener (ListenerMonad, listen, listenPatiently, say, writeToMailBox)
import MatchHelper (parseInt, readInt)
import System.Random (Random (randomRs), newStdGen)

listenForInteger :: ListenerMonad (Maybe Integer)
listenForInteger = readInt <$> listenPatiently

guess :: Integer -> ListenerMonad ()
guess secret = do
  say "Guess the number: "
  liftIO $ threadDelay 250000
  guessedNumber <- listenForInteger
  case guessedNumber of
    Nothing -> do
      say "I couldn't understand you."
      guess secret
    Just gnum ->
      if gnum > secret
        then do
          say "Too high."
          guess secret
        else
          if gnum < secret
            then do
              say "Too low."
              guess secret
            else do
              say "You got it!"
              say "Thanks for playing!"
              return ()

guessingGame :: ListenerMonad ()
guessingGame = do
  gen <- liftIO newStdGen
  let range = (1 :: Integer, 100 :: Integer)
      secret = head (randomRs range gen)
  guess secret
