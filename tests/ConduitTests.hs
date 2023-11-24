module ConduitTests (testConduit) where

import Conduit
import Control.Monad (unless, when)
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BSL
import Data.Char
import qualified Data.Conduit.Binary as CB
import Data.Maybe
import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8)
import OllamaResponseChunker
import Test.Tasty (TestTree, defaultMain, testGroup)
import Test.Tasty.HUnit (assertEqual, testCase, (@?=))

tc :: TestTree
tc = testCase "testing" $ do
  _ <- chunker
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
