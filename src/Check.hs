module Check
    ( applyCheck
    )
where

import           Flow
import           Lib
import           Rule

mineCheck :: Unit -> GameState -> Action -> Action
mineCheck unit state action =
    unitIsMiner unit action
    |> unitOnGold unit state

createCheck :: Unit -> GameState -> Action -> Action
createCheck unit state action =
    unitIsBase unit action
    |> validPos state
    |> posIsNotOccupied state
    |> goldAvailable unit state

moveCheck :: Unit -> GameState -> Action -> Action
moveCheck unit state action =
    unitIsMiner unit action
    |> validPos state
    |> posAdjacentToUnit unit
    |> posIsNotOccupied state

captureCheck :: Unit -> GameState -> Action -> Action
captureCheck unit state action =
    unitIsMiner unit action
    |> validPos state
    |> posAdjacentToUnit unit
    |> posIsOccupied state
    |> posIsOccupiedByMiner state
    |> validHeight unit

splitCheck :: Unit -> GameState -> Action -> Action
splitCheck unit state action =
    unitIsMiner unit action
    |> validPos state
    |> posAdjacentToUnit unit
    |> posIsNotOccupied state
    |> validHeight unit

attackCheck :: Unit -> GameState -> Action -> Action
attackCheck unit state action =
    unitIsMiner unit action
    |> validPos state
    |> posAdjacentToUnit unit
    |> posIsOccupied state
    |> posIsOccupiedByEnemyBase unit state

applyCheck :: Unit -> GameState -> Action -> Action
applyCheck unit state action@IDLE      = action
applyCheck unit state action@(Error _)   = action
applyCheck unit state action@(CREATE _)  = createCheck unit state action
applyCheck unit state action@MINE    = mineCheck unit state action
applyCheck unit state action@(MOVE _)    = moveCheck unit state action
applyCheck unit state action@(CAPTURE _) = captureCheck unit state action
applyCheck unit state action@(SPLIT _)   = splitCheck unit state action
applyCheck unit state action@(ATTACK _)  = attackCheck unit state action
