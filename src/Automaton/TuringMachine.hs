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

-- | Tape movements (Left | Right)
data TapeMovement = L | R deriving (Show, Eq)

-- | Transition Function
--   (state,symbol) => (state',symbol',mov)
type Transition q s = (q,s) -> Maybe (q,s,TapeMovement)

-- | Data Structure for Turing Machines
data TuringMachine q s = TuringMachine {
	getInitialState :: q,
	getBlankSymbol :: s,
	getFinalStates :: [q],
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
				delta = getTransition machine (state,n)
				Just (q',s',lr) = delta
			-- Final state, stop
			in if state `elem` getFinalStates machine then
				Just $ takeWhile (/= getBlankSymbol machine) $ tail $ reverse prev ++ next
			-- No transition
			else if delta == Nothing then Nothing
			-- Tape to right
			else if lr == R then run machine q' (s':p:ps) ns
			-- Tape to left
			else run machine q' ps (p:s':ns)

-- | Recognize
(>?>) :: (Eq q, Eq s) => [s] -> TuringMachine q s -> Bool
input >?> machine = input >>> machine /= Nothing



-- | AUXILIAR OPERATIONS

-- | Scan next symbol in the tape
--   If the tape is empty, return blank symbol
scanTape :: TuringMachine q s -> [s] -> (s,[s])
scanTape machine [] = (getBlankSymbol machine,[])
scanTape _ (h:t) = (h,t)



-- | EXAMPLES

-- | Add two numbers in base-1
--   Input: two numbers in base-1 separated by a blank (0)
--   Example: [1,1,0,1,1,1] >>> addTM = [1,1,1,1,1] = 2 + 3 = 5
addTM :: TuringMachine String Int
addTM = TuringMachine "q0" 0 ["qf"] delta
	where
		delta ("q0",1) = Just ("q0",1,R)
		delta ("q0",0) = Just ("q1",1,R)
		delta ("q1",1) = Just ("q1",1,R)
		delta ("q1",0) = Just ("q2",0,L)
		delta ("q2",1) = Just ("qf",0,L)
		delta _ = Nothing

-- | Even number of occurrences of some element
--   Input: list of elements
--   Example: "ababa" >?> evenTM 'b' = True
evenTM :: (Eq a, Bounded a, Enum a) =>  a -> TuringMachine String a
evenTM n = TuringMachine "q0" minBound ["qf"] delta
	where
		delta ("q0",x) = if x == n then Just ("q1",n,R)
		                 else if x == minBound then Just ("qf",minBound,R) 
		                 else Just ("q0",x,R)
		delta ("q1",x) = if x == n then Just ("q0",n,R)
		                 else if x == minBound then Nothing
		                 else Just ("q1",x,R)
		delta _ = Nothing
