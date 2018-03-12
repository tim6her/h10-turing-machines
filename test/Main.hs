import Test.HUnit
import Test.Framework
import Test.Framework.Providers.HUnit
import Data.Monoid
import Control.Monad

import Automaton.TuringMachine

invert :: Transition String Char
invert "start" '§' = Just ("r", '§', 1)
invert "r" '0' = Just ("r", '1', 1)
invert "r" '1' = Just ("r", '0', 1)
invert "r" '_' = Just ("halt", '_', 0)

turingInvert = TuringMachine "start" '_' "halt" invert

main :: IO ()
main = print $ "§01110" >>> turingInvert
