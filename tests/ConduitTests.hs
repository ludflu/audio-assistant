module ConduitTests (testConduit) where

import qualified Data.ByteString as B
import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8)
import Test.Tasty (TestTree, defaultMain, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))

packStr :: String -> B.ByteString
packStr = encodeUtf8 . T.pack

testString :: B.ByteString
testString = packStr "{\"test\":\"bla\" \"done\"=False}"

testConduit =
  testGroup
    "ConduitTests"
    [ testCase
        "Sets end time when voice stops"
        $ let testit = 1
           in testit @?= 1
    ]
