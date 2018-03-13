{-|
    Module      : Automaton.TuringMachine
    Description : Turing Machines
    Copyright   : (c) Tim B. Herbstrith, 2018
    License     : MIT license
    Maintainer  : tim6her
    Stability   : experimental
    Portability : portable
    Language    : Haskell2010
    GitHub      : https://github.com/tim6her/h10-turing-machines

    A Turing machine is an abstract machine that manipulates symbols
    on a strip of tape according to a table of rules.
-}

module Automaton.TuringMachine (
  TuringMachine(..),
  Transition,
  TapeMovement,
  toTransition,
  (>>>),
  (>?>),
  (>.>)
) where

import Control.Exception

-- | DATA STRUCTURES AND TYPES

-- | Tape movements
type TapeMovement = Int

-- | Transition Function
--   state symbol => (state', symbol', mov)
type Transition q s = q -> s -> Maybe (q, s, TapeMovement)

-- | Data Structure for Turing Machines
data TuringMachine q s = TuringMachine {
  getInitialState :: q,
  getBlankSymbol :: s,
  getFinalState :: q,
  getTransition :: Transition q s
}

-- | Transform a function to a transition function
toTransition :: (Eq q) => (q -> s -> (q, s, TapeMovement)) ->
                q -> -- error state
                Transition q s
toTransition f err state char
  | q' == err = Nothing
  | otherwise = Just (q', s', move)
  where (q', s', move) = f state char

-- | RUN

-- | Calculate
(>>>) :: (Eq q, Eq s) => [s] -> TuringMachine q s -> Maybe [s]
input >>> machine = run machine (getInitialState machine) [] input
  where
    run machine state prev next = let
        (p,ps) = scanTape machine prev
        (n,ns) = scanTape machine next
        delta = getTransition machine state n
        Just (q', s', move) = delta
      -- Final state, stop
      in if state == getFinalState machine then
        Just $ takeWhile (/= getBlankSymbol machine) $ tail $ reverse prev ++ next
      -- No transition
      else if delta == Nothing then Nothing
      -- Tape to right
      else if move == 1 then run machine q' (s':p:ps) ns
      -- Tape to left
      else if move == (-1) then run machine q' ps (p:s':ns)
      -- Tape doesn't move
      else if move == 0 then run machine q' (p:ps) (s':ns)
      else Nothing

-- | Recognize
(>?>) :: (Eq q, Eq s) => [s] -> TuringMachine q s -> Bool
input >?> machine = input >>> machine /= Nothing

-- | Compose
(>.>) :: (Eq q, Eq s) => Maybe [s] ->
                         TuringMachine q s ->
                         Maybe [s]
Just xs >.> machine = xs >>> machine
Nothing >.> _ = Nothing

-- | AUXILIAR OPERATIONS

-- | Scan next symbol in the tape
--   If the tape is empty, return blank symbol
scanTape :: TuringMachine q s -> [s] -> (s,[s])
scanTape machine [] = (getBlankSymbol machine,[])
scanTape _ (h:t) = (h,t)
