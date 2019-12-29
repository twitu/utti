module Consequence
    ( applyConsequenceFunc
    )
where

import           Lib
import           Construct
import           System.IO
import qualified Data.Map.Strict               as Map
import           Data.Map                       ( Map() )
import           Flow
import           Control.Monad.Writer

-- change functions modify game state and return a new copy

-- change game gold with given amount and log change
changeGold :: Gold -> GameState -> Writer [String] GameState
changeGold amount GameState { gamequeue = a, gamegold = gold, gameunitboard = b }
    = writer (GameState a newgold b, ["changeGold " ++ show gold])
  where
    newgold = Gold ((redgold amount +) $ redgold gold)
                   ((bluegold amount +) $ bluegold gold)

-- remove unit at given position from unit board and log change
removeUnit :: Pos -> GameState -> Writer [String] GameState
removeUnit pos GameState { gamequeue = a, gamegold = b, gameunitboard = board }
    = writer
        (GameState a b (Map.delete pos board), ["removeUnit " ++ show remunit])
    where remunit = (Map.!) board pos

-- add unit at given position to unit board and log change
addUnit :: Pos -> Unit -> GameState -> Writer [String] GameState
addUnit pos unit GameState { gamequeue = a, gamegold = b, gameunitboard = board }
    = writer
        (GameState a b (Map.insert pos unit board), ["addUnit " ++ show unit])

-- add unit to its team queue and log change
addToQueue :: Unit -> GameState -> Writer [String] GameState
addToQueue unit@(Unit _ _ Red _ _) GameState { gamequeue = queue, gamegold = a, gameunitboard = b }
    = writer (GameState newqueue a b, ["addToQueue Red " ++ show unit])
    where newqueue = Queue (redqueue queue ++ [unit]) (bluequeue queue)
addToQueue unit@(Unit _ _ Blue _ _) GameState { gamequeue = queue, gamegold = a, gameunitboard = b }
    = writer (GameState newqueue a b, ["addToQueue Blue " ++ show unit])
    where newqueue = Queue (redqueue queue ++ [unit]) (bluequeue queue)

-- consequence functions make changes to game state and log them for an action

-- idle action has no consequence
idleConsequence :: Action -> GameState -> Writer [String] GameState
idleConsequence action = return

-- create action adds a unit to the board and queue
createConsequence :: Action -> GameState -> Writer [String] GameState
createConsequence Action { actionunit = (Unit _ _ team _ _), actionpos = Just pos } state
    = changeGold updategold state >>= addUnit pos newUnit >>= addToQueue newUnit
  where
    updategold = if team == Red then Gold (-10) 0 else Gold 0 (-10)
    newUnit    = Unit pos 1 team Troop constructUnitId

-- move action moves a unit
moveConsequence :: Action -> GameState -> Writer [String] GameState
moveConsequence Action { actionpos = Just newpos, actionunit = unit } state =
    removeUnit curpos state >>= addUnit newpos movedunit
  where
    (Unit curpos b c d e) = unit
    movedunit             = Unit newpos b c d e

-- capture action creates a new unit by stacking captured and capturor unit
captureConsequence :: Action -> GameState -> Writer [String] GameState
captureConsequence Action { actionpos = Just capturepos, actionunit = unit } state
    = removeUnit (pos unit) state
        >>= removeUnit capturepos
        >>= addUnit capturepos newunit
  where
    enemyunit = gameunitboard state Map.! capturepos
    newunit =
        Unit capturepos (height enemyunit + 1) (team unit) Troop (unitid unit)

-- climb action creates a new unit by stacking a friendly unit on top of another
climbConsequence :: Action -> GameState -> Writer [String] GameState
climbConsequence Action { actionpos = Just capturepos, actionunit = unit } state
    = removeUnit (pos unit) state
        >>= removeUnit capturepos
        >>= addUnit capturepos newunit
  where
    friendlyunit = gameunitboard state Map.! capturepos
    newunit      = Unit capturepos
                        (height friendlyunit + 1)
                        (team unit)
                        Troop
                        (unitid unit)

-- mine action mines gold in proportion to height of unit
mineConsequence :: Action -> GameState -> Writer [String] GameState
mineConsequence Action { actionpos = Just minepos, actionunit = Unit _ height team _ _ }
    = changeGold updategold
  where
    updategold =
        if team == Red then Gold (10 * height) 0 else Gold 0 (10 * height)

-- split action splits a unit in half and adds the newunit to game queue
splitConsequnce :: Action -> GameState -> Writer [String] GameState
splitConsequnce Action { actionpos = Just splitpos, actionunit = unit@(Unit curpos height team _ curid) } state
    = removeUnit curpos state
        >>= addUnit curpos   newunit
        >>= addUnit splitpos oldunit
        >>= addToQueue newunit
  where
    oldunit = Unit splitpos (height `div` 2) team Troop curid
    newunit = Unit curpos (height - height `div` 2) team Troop constructUnitId

-- chooses which consequence to apply based on action
applyConsequenceFunc :: GameState -> Action -> (GameState, [String])
applyConsequenceFunc state action@(Action IDLE _ _) =
    runWriter $ idleConsequence action state
applyConsequenceFunc state action@(Action CREATE_TROOP _ _) =
    runWriter $ createConsequence action state
applyConsequenceFunc state action@(Action MINE _ _) =
    runWriter $ mineConsequence action state
applyConsequenceFunc state action@(Action MOVE _ _) =
    runWriter $ moveConsequence action state
applyConsequenceFunc state action@(Action CAPTURE _ _) =
    runWriter $ captureConsequence action state
applyConsequenceFunc state action@(Action CLIMB _ _) =
    runWriter $ climbConsequence action state
applyConsequenceFunc state action@(Action SPLIT _ _) =
    runWriter $ splitConsequnce action state
