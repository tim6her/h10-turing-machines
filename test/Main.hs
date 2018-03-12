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

turingInvert = TuringMachine "start" '_' "halt" invert

testTuring = testCase
  "Testing turingInvert" $
    assertEqual [] (Just "§01101101") ("§10010010" >>> turingInvert)

testReverse = testCase
  "Test that invert is self inverse" $
    assertEqual
      [] (Just "§010111") ("§010111" >>> turingInvert >>= (>>> turingInvert))

unitTests =
  testGroup
    "Unit tests" [testTuring, testReverse]

main :: IO ()
main = defaultMain unitTests
