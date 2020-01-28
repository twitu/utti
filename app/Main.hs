module Main where

import           System.IO
import           Lib
import           Check
import           Consequence
import           Construct
import           Data.Maybe

main :: IO ()
main = do
  playRounds 0 constructInitialGameState

getInputAction :: IO (ActionName, Maybe Pos)
getInputAction = readLine

createAction :: Unit -> (ActionName, Maybe Pos) -> Maybe Action
createAction unit (name, maybepos) = return (Action name maybepos unit)

playRounds :: Int -> GameState -> GameState
playRounds roundnum state = if roundnum < 500
  then playRounds (roundnum + 1) newstate
  else state
  where newstate = reconstructGameState $ playRound state

playRound :: (GameState, Unit) -> (GameState, Unit)
playRound (state, prevunit) = if endRound state
  then (state, prevunit)
  else playRound (newstate, turnunit)
 where
  (tempstate, turnunit) = getNextTurnUnit $ nextTeamTurn prevunit state
  (newstate , log     ) = if isJust $ applyCheck tempstate turnaction
    then applyConsequenceFunc tempstate turnaction
    else (tempstate, [])

playTurn :: (GameState, Unit) -> (GameState, [String])
playTurn curstate unit = do
  inputaction <- getInputAction
  if isJust $ applyCheck curstate $ createAction inputaction
    then applyConsequenceFunc curstate 
  where
    turnaction = createAction inputaction


endRound :: GameState -> Bool
endRound state@(GameState (Queue redqueue bluequeue) a b) =
  null redqueue && null bluequeue

getNextTurnUnit :: Team -> GameState -> (GameState, Unit)
getNextTurnUnit Red state@(GameState (Queue redqueue bluequeue) a b) =
  if null redqueue
    then getNextTurnUnit Blue state
    else (GameState newqueue a b, nextunit)
 where
  nextunit = head redqueue
  newqueue = Queue (tail redqueue) bluequeue
getNextTurnUnit Blue state@(GameState (Queue redqueue bluequeue) a b) =
  if null bluequeue
    then getNextTurnUnit Red state
    else (GameState newqueue a b, nextunit)
 where
  nextunit = head bluequeue
  newqueue = Queue redqueue (tail bluequeue)

printTurnLog :: [String] -> [IO ()]
printTurnLog = map putStrLn

nextTeamTurn :: Unit -> Team
nextTeamTurn Unit { team = Red }  = Blue
nextTeamTurn Unit { team = Blue } = Red
