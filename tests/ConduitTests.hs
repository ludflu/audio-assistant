module ConduitTests (testConduit) where

import Conduit
import Control.Monad (unless, when)
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.UTF8 as BSU
import Data.Char
import qualified Data.Conduit.Binary as CB
import Data.Maybe
import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8)
import OllamaApi (chunker)
import Test.Tasty (TestTree, defaultMain, testGroup)
import Test.Tasty.HUnit (assertEqual, testCase, (@?=))

chunkerTest :: IO ()
chunkerTest = do
  s <- readFile "test.json"
  print "Starting"
  runConduitRes $
    yield (BSU.fromString s)
      .| chunker (liftIO . print . ("Processing chunk: " ++) . show)

tc :: TestTree
tc = testCase "testing" $ do
  chunkerTest
  assertEqual "expected" expected ()

expected = ()

testConduit =
  testGroup
    "ConduitTests"
    [ testCase
        "one equals one"
        $ let testit = 1
           in testit @?= 1,
      tc
    ]
