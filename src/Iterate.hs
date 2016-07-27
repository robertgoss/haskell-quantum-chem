module Iterate(
  iterateUntil, iterateUntilM,
  iterateDelta, iterateDeltaM
) where
    
    
import Control.Monad
    

-- Some modules used for simulation by iterating until a desired result is reached
-- Takes a state and a transition function and repeatedly applies it until a condition
-- is reached 
-- Either on the state (iterateUntil) or between states (iterateDelta)

-- In this we put all the local and global state into a single state variable
--  (Global state is unchanging). We also provide monadic versions of both of these.

-- We note the iteration does not force full strict evaluation
-- of the state at each iteration. Only the amount of state needed to evaluate
-- the property will neccessarly be evaulated and thus lazyness can build up a 
-- lot of memory keeping thunks of the previous unevauluated parts of state.
-- If this an issue make sure the transition function forces a full strict evaluation 
-- of the state.


-- We give some type defintions for the functional arguments to make the later statements
--  easier to read

-- A transition to (hopefully) improve the state
type Transition state = state -> state
type TransitionM m state = state -> m state

type Property state = state -> Bool
type Property2 state = (state, state) -> Bool


-- Iteratively apply the transition function starting with the initial
-- state until the property holds
iterateUntil :: Property state -> Transition state -> state -> state
iterateUntil prop transition initial 
 | prop initial = initial
 | otherwise = iterateUntil prop transition (transition initial) 


-- Iteratively apply the monadic transition function starting with the initial
-- state until the property holds
iterateUntilM :: (Monad m) => Property state -> TransitionM m state -> state -> m state
iterateUntilM prop transition initial 
 | prop initial = return initial
 | otherwise = transition initial >>= iterateUntilM prop transition


-- Iteratively apply the transition function starting with the initial
-- state until the property holds for the last 2 states. 
-- This is mostly used to iterate until 2 states are the same.
iterateDelta :: Property2 state -> Transition state -> state -> state
iterateDelta prop transition initial = fst $ iterateUntil prop transitionPair firstPair
   -- Use iterateUntil on the state which is the pair of the state and the previous state
   -- the transition function updates the both element of the pair.
   where transitionPair (state, prevState) = (transition state, state)
         firstPair = (transition initial, initial)

-- Iteratively apply the transition function starting with the initial
-- state until the property holds for the last 2 states. 
-- This is mostly used to iterate until 2 states are the same.
iterateDeltaM :: (Monad m) => Property2 state -> TransitionM m state -> state -> m state
iterateDeltaM prop transition initial = fmap fst (firstPair >>= iterateUntilM prop transitionPair)
   -- Use iterateUntil on the state which is the pair of the state and the previous state
   -- the transition function updates the both element of the pair.
   where transitionPair (state, prevState) = do nextState <- transition state
                                                return (nextState, state)
         firstPair = do secondState <- transition initial
                        return (secondState, initial)
