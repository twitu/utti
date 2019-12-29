module Main where

import           System.IO
import           Lib
import           Check
import           Consequence
import           Construct
import           Data.Maybe

type TurnUnits = (Maybe Unit, Maybe Unit)

main :: IO ()
main = undefined

getInputAction :: (ActionName, Maybe Pos)
getInputAction = undefined

playRound :: GameState -> GameState
playRound = undefined

getNextTurnUnits :: GameState -> (GameState, TurnUnits)
getNextTurnUnits state@(GameState (Queue redqueue bluequeue) a b) =
    (GameState newqueue a b, turnunits)
  where
    redhead   = listToMaybe redqueue
    bluehead  = listToMaybe bluequeue
    redtail   = if null redqueue then [] else tail redqueue
    bluetail  = if null bluequeue then [] else tail bluequeue
    newqueue  = Queue redtail bluetail
    turnunits = (redhead, bluehead)

playTurn :: TurnUnits -> GameState -> (GameState, [String])
playTurn (Just redunit, Just blueunit) state = (newState, otherlog ++ partlog)
  where
    (betweenstate, partlog ) = playTurn (Just redunit, Nothing) state
    (newState    , otherlog) = playTurn (Nothing, Just blueunit) state
playTurn (Just redunit, Nothing) state = case checkedaction of
    Just checkedaction -> applyConsequenceFunc state checkedaction
    Nothing            -> (state, [])
  where
    (actionname, maybepos) = getInputAction
    action                 = Action actionname maybepos redunit
    checkedaction          = applyCheck state action
playTurn (Nothing, Just blueunit) state = case checkedaction of
    Just checkedaction -> applyConsequenceFunc state checkedaction
    Nothing            -> (state, [])
  where
    (actionname, maybepos) = getInputAction
    action                 = Action actionname maybepos blueunit
    checkedaction          = applyCheck state action
playTurn (Nothing, Nothing) state = undefined  --end round here

printTurnLog :: [String] -> [IO ()]
printTurnLog = map putStrLn
