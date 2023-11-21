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
import Test.Tasty (TestTree, defaultMain, testGroup)
import Test.Tasty.HUnit (assertEqual, testCase, (@?=))

packStr :: String -> B.ByteString
packStr = encodeUtf8 . T.pack

testString :: BSL.ByteString
testString = BSL.fromStrict $ packStr "{\"test\":\"1 bla\" \"done\"=False}{\"test\":\"2 bla\" \"done\"=False}{\"test\":\"3 bla\" \"done\"=False}"

runc :: IO ()
runc =
  runConduitRes $
    yield testString
      .| conduitChunks '}'
      .| mapM_C (liftIO . putStrLn . ("Processing chunk: " ++) . show)

expected = ()

conduitChunks :: Monad m => Char -> ConduitT BSL.ByteString BSL.ByteString m ()
conduitChunks trigger = do
  let triggerByte = fromIntegral (fromEnum trigger)
  awaitForever $ \bs -> do
    let (prefix, suffix) = BSL.break (== triggerByte) bs
    unless (BSL.null prefix) $ yield prefix
    when (not (BSL.null suffix)) $ do
      let rest = BSL.drop 1 suffix
      unless (BSL.null rest) $ leftover rest

tc :: TestTree
tc = testCase "testing" $ do
  _ <- runc
  assertEqual "expected" expected ()

testConduit =
  testGroup
    "ConduitTests"
    [ testCase
        "one equals one"
        $ let testit = 1
           in testit @?= 1,
      tc
    ]
