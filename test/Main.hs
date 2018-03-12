import Test.Tasty
import Test.Tasty.HUnit

import Automaton.TuringMachine

invert :: Transition String Char
invert "start" '§' = Just ("r", '§', 1)
invert "r" '0' = Just ("r", '1', 1)
invert "r" '1' = Just ("r", '0', 1)
invert "r" '_' = Just ("halt", '_', 0)

invert' :: String -> Char -> (String, Char, TapeMovement)
invert' "start" '§' = ("r", '§', 1)
invert' "r" '0' = ("r", '1', 1)
invert' "r" '1' = ("r", '0', 1)
invert' "r" '_' = ("halt", '_', 0)

addTM :: TuringMachine String Int
addTM = TuringMachine "q0" 0 "qf" delta
  where
    delta "q0" 1 = Just ("q0", 1, 1)
    delta "q0" 0 = Just ("q1", 1, 1)
    delta "q1" 1 = Just ("q1", 1, 1)
    delta "q1" 0 = Just ("q2", 0, (-1))
    delta "q2" 1 = Just ("qf", 0, (-1))
    delta _q _s = Nothing

turingInvert = TuringMachine "start" '_' "halt" invert

testTuring = testCase
  "Testing turingInvert" $
    assertEqual [] (Just "§01101101") ("§10010010" >>> turingInvert)

testReverse = testCase
  "Test that invert is self inverse" $
    assertEqual
      [] (Just "§010111") ("§010111" >>> turingInvert >>= (>>> turingInvert))

testAddTM1 = testCase
  "Test adding two numbers in tally notation - 1" $
    assertEqual
      [] (Just [1,1,1]) ([0,1,1,1] >>> addTM)

testAddTM2 = testCase
  "Test adding two numbers in tally notation - 2" $
    assertEqual
      [] (Just [1,1,1]) ([1,1,0,1] >>> addTM)

testAddTM3 = testCase
  "Test adding two numbers in tally notation - 3" $
    assertEqual
      [] (Just [1,1,1,1,1]) ([1,1,0,1,1,1] >>> addTM)

unitTests =
  testGroup
    "Unit tests" [testTuring,
                  testReverse,
                  testAddTM1,
                  testAddTM2,
                  testAddTM3]

main :: IO ()
main = defaultMain unitTests
