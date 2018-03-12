{-|
    Module      : Automaton.TuringMachine
    Description : Turing Machines
    Copyright   : (c) Jose Antonio Riaza Valverde, 2016
    License     : MIT license
    Maintainer  : riazavalverde@gmail.com
    Stability   : experimental
    Portability : portable
    Website     : http://riazavalverde.com
    GitHub      : https://github.com/jariazavalverde/hackage-turing-machines

    A Turing machine is an abstract machine that manipulates symbols
    on a strip of tape according to a table of rules.
-}

module Automaton.TuringMachine (
  TuringMachine(..),
  Transition,
  TapeMovement,
  (>>>),
  (>?>)
) where



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
      else run machine q' (p:ps) (s':ns)

-- | Recognize
(>?>) :: (Eq q, Eq s) => [s] -> TuringMachine q s -> Bool
input >?> machine = input >>> machine /= Nothing



-- | AUXILIAR OPERATIONS

-- | Scan next symbol in the tape
--   If the tape is empty, return blank symbol
scanTape :: TuringMachine q s -> [s] -> (s,[s])
scanTape machine [] = (getBlankSymbol machine,[])
scanTape _ (h:t) = (h,t)
