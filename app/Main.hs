module Main where

import           System.IO
import           Lib
import           Check
import           Consequence
import           Construct
import           Data.Maybe

main :: IO (GameState, Team)
main = do
  print $ show constructInitialGameState
  playRounds 0 constructInitialGameState Red

playRounds :: Int -> GameState -> Team -> IO (GameState, Team)
playRounds roundnum state curteam = do
  print $ "Round " ++ show roundnum
  (tempstate, nextteam) <- playRound (state, curteam)
  if roundnum == 500
  then return (tempstate, nextteam)
  else 
    playRounds (roundnum + 1) (reconstructGameState tempstate) nextteam

playRound :: (GameState, Team) -> IO (GameState, Team)
playRound (state, curteam) = do
  newstate <- playTurn tempstate turnunit
  if endRound newstate
  then return (newstate, nextTeam)
  else playRound (newstate, nextTeam)
 where
    (tempstate, turnunit) = getNextTurnUnit curteam state
    nextTeam = nextTeamTurn $ team turnunit

playTurn :: GameState -> Unit -> IO GameState
playTurn curstate unit = do
    print $ show unit
    inputaction <- readLn
    case applyCheck unit curstate inputaction of
      Error message -> printTurnLog (curstate, [message])
      _ -> printTurnLog $ applyConsequenceFunc unit curstate inputaction

endRound :: GameState -> Bool
endRound state@GameState {queue = Queue redqueue bluequeue} =
  null redqueue && null bluequeue

getNextTurnUnit :: Team -> GameState -> (GameState, Unit)
getNextTurnUnit Red state@GameState {queue = Queue redqueue bluequeue} =
  if null redqueue
    then getNextTurnUnit Blue state
    else (state {queue = newqueue}, nextunit)
 where
  nextunit = head redqueue
  newqueue = Queue (tail redqueue) bluequeue
getNextTurnUnit Blue state@GameState {queue = Queue redqueue bluequeue} =
  if null bluequeue
    then getNextTurnUnit Red state
    else (state {queue = newqueue}, nextunit)
 where
  nextunit = head bluequeue
  newqueue = Queue redqueue (tail bluequeue)

printTurnLog :: (GameState, [String]) -> IO GameState
printTurnLog (state, log) = do
  mapM_ putStrLn log
  return state

nextTeamTurn :: Team -> Team
nextTeamTurn Red  = Blue
nextTeamTurn Blue = Red
