# h10-turing-machines
A simple simulator for Turing machines based on [hackage-turing-machines](https://github.com/jariazavalverde/hackage-turing-machines)

[![Build Status](https://travis-ci.org/tim6her/h10-turing-machines.svg?branch=master)](https://travis-ci.org/tim6her/h10-turing-machines)


## Install
```
caball install turing-machines
```
## Data Structures and Types
```haskell
-- | Tape movements (Left | Right)
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
```
## Functions
### `(>>>)`
Run Turing machine to calculate.
```haskell
(>>>) :: (Eq q, Eq s) => [s] -> TuringMachine q s -> Maybe [s]
```
#### Example: add two numbers in base-1
```haskell
addTM :: TuringMachine String Int
addTM = TuringMachine "q0" 0 "qf" delta
  where
    delta "q0" 1 = Just ("q0", 1, 1)
    delta "q0" 0 = Just ("q1", 1, 1)
    delta "q1" 1 = Just ("q1", 1, 1)
    delta "q1" 0 = Just ("q2", 0, (-1))
    delta "q2" 1 = Just ("qf", 0, (-1))
    delta _q _s = Nothing
```
```
[0,1,1,1] >>> addTM = [1,1,1]
[1,1,0,1] >>> addTM = [1,1,1]
[1,1,0,1,1,1] >>> addTM = [1,1,1,1,1]
```

### `(>?>)`
Run Turing machine to recognise.
```haskell
(>?>) :: (Eq q, Eq s) => [s] -> TuringMachine q s -> Bool
```

### `(>.>)`
Compose Turing machines
```haskell
(>.>) :: (Eq q, Eq s) => Maybe [s] ->
                         TuringMachine q s ->
                         Maybe [s]
```

<!--
#### Example: even number of occurrences of some element
```haskell
evenTM :: (Eq a, Bounded a, Enum a) =>  a -> TuringMachine String a
evenTM n = TuringMachine "q0" minBound "qf" delta
  where
    delta  "q0" x = if x == n then Just ("q1", n, R)
                    else if x == minBound then Just ("qf", minBound, R)
                    else Just ("q0", x, R)
    delta "q1" x = if x == n then Just ("q0", n, R)
                   else if x == minBound then Nothing
                   else Just ("q1", x, R)
    delta _q _s = Nothing
```
```
"cabababac" >?> evenTM 'a' = True
"cabababac" >?> evenTM 'b' = False
"cabababac" >?> evenTM 'c' = True
```
-->
