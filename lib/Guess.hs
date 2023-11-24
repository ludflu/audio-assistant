{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Guess where

import Control.Concurrent (threadDelay)
import Control.Monad.State (liftIO)
import Data.Char (isNumber, toLower)
import Listener (ListenerMonad, listen, writeToMailBox)
import MatchHelper (parseInt, readInt)
import System.Random (Random (randomRs), newStdGen)

listenForInteger :: ListenerMonad (Maybe Integer)
listenForInteger = readInt <$> listen

guess :: Integer -> ListenerMonad ()
guess secret = do
  writeToMailBox "Guess the number: "
  liftIO $ threadDelay 250000
  guessedNumber <- listenForInteger
  case guessedNumber of
    Nothing -> do
      writeToMailBox "I couldn't understand you."
      guess secret
    Just gnum ->
      if gnum > secret
        then do
          writeToMailBox "Too high."
          guess secret
        else
          if gnum < secret
            then do
              writeToMailBox "Too low."
              guess secret
            else do
              writeToMailBox "You got it!"
              writeToMailBox "Thanks for playing!"

guessingGame :: ListenerMonad ()
guessingGame = do
  gen <- liftIO newStdGen
  let range = (1 :: Integer, 100 :: Integer)
      secret = head (randomRs range gen)
  guess secret
