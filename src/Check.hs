module Check
    ( applyCheck
    )
where

import           Rule
import           Lib

createCheck :: GameState -> Action -> Maybe Action
createCheck state action =
    unitIsBase action
        >>= validPos
        >>= posIsNotOccupied state
        >>= goldAvailable state

moveCheck :: GameState -> Action -> Maybe Action
moveCheck state action =
    unitIsTroop action
        >>= validPos
        >>= posAdjacentToUnit
        >>= posIsNotOccupied state

captureCheck :: GameState -> Action -> Maybe Action
captureCheck state action =
    unitIsTroop action
        >>= validPos
        >>= posAdjacentToUnit
        >>= posIsOccupied state
        >>= posIsOccupiedByTroop state
        >>= isEnemyOnPos state
        >>= validHeight state

climbCheck :: GameState -> Action -> Maybe Action
climbCheck state action =
    unitIsTroop action
        >>= validPos
        >>= posAdjacentToUnit
        >>= posIsOccupied state
        >>= posIsOccupiedByTroop state
        >>= isFriendlyOnPos state
        >>= validHeight state

mineCheck :: GameState -> Action -> Maybe Action
mineCheck state action = unitIsTroop action >>= validPos >>= unitOnGold state

splitCheck :: GameState -> Action -> Maybe Action
splitCheck state action =
    unitIsTroop action
        >>= validPos
        >>= posAdjacentToUnit
        >>= posIsNotOccupied state
        >>= validHeight state

idleCheck :: GameState -> Action -> Maybe Action
idleCheck state = Just

applyCheck :: GameState -> Action -> Maybe Action
applyCheck state action@(Action IDLE         _ _) = idleCheck state action
applyCheck state action@(Action CREATE_TROOP _ _) = createCheck state action
applyCheck state action@(Action MINE         _ _) = mineCheck state action
applyCheck state action@(Action MOVE         _ _) = moveCheck state action
applyCheck state action@(Action CAPTURE      _ _) = captureCheck state action
applyCheck state action@(Action CLIMB        _ _) = climbCheck state action
applyCheck state action@(Action SPLIT        _ _) = splitCheck state action
