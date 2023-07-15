import Listener
import Test.Tasty (TestTree, defaultMain, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))

main :: IO ()
main = defaultMain testCalcBoundary

testCalcBoundary :: TestTree
testCalcBoundary =
  testGroup
    "boundary tests"
    [ testCase "Sets start time when voice detected" $
        let listener = ListenerState {voiceEndTime = Nothing, voiceStartTime = Nothing, timeOffset = 0.0}
            activations = [True, True, True]
            elapsed = 3.0
            threshold = 0.5
            expected = RecordingBound {voiceStart = Just 0.0, voiceEnd = Nothing}
            boundary = calcBoundary listener activations elapsed threshold
         in voiceStart boundary @?= voiceStart expected,
      testCase
        "Sets end time when voice stops"
        $ let listener = ListenerState {voiceEndTime = Nothing, voiceStartTime = Just 0.0, timeOffset = 0.0}
              activations = [True, True, True, True, False, False, False, False, False]
              elapsed = 3.0
              threshold = 0.5
              expected = RecordingBound {voiceStart = Just 0.0, voiceEnd = Just 3.0}
              boundary = calcBoundary listener activations elapsed threshold
           in voiceEnd boundary @?= voiceEnd expected
    ]

tests :: TestTree
tests = testGroup "my tests" [testCase "Simple test" $ True @?= True]