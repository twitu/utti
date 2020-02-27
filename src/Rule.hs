module Rule
    ( goldAvailable
    , unitOnGold
    , posIsOccupied
    , posIsNotOccupied
    , posIsOccupiedByMiner
    , posIsOccupiedByEnemyBase
    , posAdjacentToUnit
    , unitIsBase
    , unitIsMiner
    , validHeight
    , validPos
    )
where

import           Lib
import           Data.Maybe
import           Construct

-- defining constants
unitGold = 10

-- check if unit performing action is of type base
unitIsBase :: Unit -> Action -> Action
unitIsBase Unit{unittype = Base val} action = action
unitIsBase _ _ = Error "Unit is must be of type Base to perform the action"

-- check if unit performing action is of type miner
unitIsMiner :: Unit -> Action -> Action
unitIsMiner Unit{unittype = Miner val} action = action
unitIsMiner _ _ = Error "Unit is must be of type Miner to perform the action"

-- check if position where action will be performed is valid
validPos :: GameState -> Action -> Action
validPos GameState {bound = dim} action = case action of
    CREATE curpos -> validPosCheck curpos action dim
    MOVE curpos -> validPosCheck curpos action dim
    CAPTURE curpos -> validPosCheck curpos action dim
    SPLIT curpos -> validPosCheck curpos action dim
    ATTACK curpos -> validPosCheck curpos action dim
    Error _ -> action
    _ -> Error "Action not supported"

validPosCheck :: Pos -> Action -> Int -> Action
validPosCheck (row, col) action dim =
    if 0 <= row && row < dim && 0 <= col && col < dim  -- board limits
    then action
    else Error "Action position not in board bounds"

-- checks if target position is occupied by any unit
posIsOccupied :: GameState -> Action -> Action
posIsOccupied state action = case action of
    CREATE curpos -> posIsOccupiedCheck state curpos action
    MOVE curpos -> posIsOccupiedCheck state curpos action
    CAPTURE curpos -> posIsOccupiedCheck state curpos action
    SPLIT curpos -> posIsOccupiedCheck state curpos action
    ATTACK curpos -> posIsOccupiedCheck state curpos action
    Error _ -> action
    _ -> Error "Action not supported"

posIsOccupiedCheck :: GameState -> Pos -> Action -> Action
posIsOccupiedCheck state target action =
    let
        units = gameunits state
    in
        if any (\unit -> pos unit == target) units
        then action
        else Error "Target position in occupied"

-- checks if target position is not occupied by any unit
posIsNotOccupied :: GameState -> Action -> Action
posIsNotOccupied state action = case action of
    CREATE curpos -> posIsNotOccupiedCheck state curpos action
    MOVE curpos -> posIsNotOccupiedCheck state curpos action
    CAPTURE curpos -> posIsNotOccupiedCheck state curpos action
    SPLIT curpos -> posIsNotOccupiedCheck state curpos action
    ATTACK curpos -> posIsNotOccupiedCheck state curpos action
    Error _ -> action
    _ -> Error "Action not supported"

posIsNotOccupiedCheck :: GameState -> Pos -> Action -> Action
posIsNotOccupiedCheck state target action =
    let
        units = gameunits state
    in
        if all (\unit -> pos unit /= target) units
        then action
        else Error "Target position in not occupied"

-- checks if position is occupied by a miner
-- Note: should only be called after posIsOccupied; will throw runtime error if pos is unoccupied
posIsOccupiedByMiner :: GameState -> Action -> Action
posIsOccupiedByMiner state action = case action of
    CAPTURE curpos -> posIsOccupiedByMinerCheck state curpos action
    Error _ -> action
    _ -> Error "Action not supported"

posIsOccupiedByMinerCheck :: GameState -> Pos -> Action -> Action
posIsOccupiedByMinerCheck state target action =
    let
        units = gameunits state
    in
        if any (\unit -> pos unit == target && Miner{} == unittype unit) units
        then action
        else Error "Target position is not occupied by miner"

-- checks if position is occupied by a miner
-- Note: should only be called after posIsOccupied; will throw runtime error if pos is unoccupied
posIsOccupiedByEnemyBase :: Unit -> GameState -> Action -> Action
posIsOccupiedByEnemyBase unit state action = case action of
    ATTACK curpos -> posIsOccupiedByEnemyBaseCheck unit state curpos action
    Error _ -> action
    _ -> Error "Action not supported"

posIsOccupiedByEnemyBaseCheck :: Unit -> GameState -> Pos -> Action -> Action
posIsOccupiedByEnemyBaseCheck curunit state target action =
    let
        units = gameunits state
    in
        if any (\unit -> pos unit == target && Base{} == unittype unit && team unit /= team curunit) units
        then action
        else Error "Target position is not occupied by miner"

-- checks if unit performing action is on a gold mine
-- Note: call only after checking valid position
unitOnGold :: Unit -> GameState -> Action -> Action
unitOnGold unit state action = case action of
    MINE -> unitOnGoldCheck unit state action
    Error _ -> action
    _ -> Error "Action not supported"

unitOnGoldCheck :: Unit -> GameState -> Action -> Action
unitOnGoldCheck unit state action =
    if (!!) goldmines flattenedPos
    then action
    else Error "Unit not on gold"
    where
        goldmines = goldMine state
        dimension = bound state
        (i, j) = pos unit
        flattenedPos = i*dimension + j

-- check different levels of gold availability based on action action
goldAvailable :: Unit -> GameState -> Action -> Action
goldAvailable unit state action = case action of
    CREATE _ -> goldAvailableCheck unit state action
    Error _ -> action
    _ -> Error "Action not supported"

goldAvailableCheck :: Unit -> GameState -> Action -> Action
goldAvailableCheck unit state action
    | team unit == Red =
        if redgold (gold state) >= unitGold
        then action
        else Error "Not enough gold"
    | otherwise =
        if bluegold (gold state) >= unitGold
        then action
        else Error "Not enough gold"

-- checks if pos where action will be performed is adjacent to unit performing action
posAdjacentToUnit :: Unit -> Action -> Action
posAdjacentToUnit unit action = case action of
    CREATE curpos -> posAdjacentToUnitCheck unit curpos action
    MOVE curpos -> posAdjacentToUnitCheck unit curpos action
    CAPTURE curpos -> posAdjacentToUnitCheck unit curpos action
    SPLIT curpos -> posAdjacentToUnitCheck unit curpos action
    ATTACK curpos -> posAdjacentToUnitCheck unit curpos action
    Error _ -> action
    _ -> Error "Action not supported"

posAdjacentToUnitCheck :: Unit -> Pos -> Action -> Action
posAdjacentToUnitCheck Unit {pos = (x, y)} (row, col) action =
    let dx = row - x
        dy = col - y
    in
        if -1 <= dx && dx <= 1 && -1 <= dy && dy <= 1 && (dx /= 0 || dy /= 0)
        then action
        else Error "Unit is not adjacent to target position"

-- checks if unit has valid height to perform action
-- Note: should only be called after checking that unit is miner
validHeight :: Unit -> Action -> Action
validHeight unit action = case action of
    CAPTURE _ -> validHeightCheck action unit
    SPLIT _ -> validHeightCheck action unit
    Error _ -> validHeightCheck action unit
    _ -> Error "Action not supported"

validHeightCheck :: Action -> Unit -> Action
validHeightCheck action@(CAPTURE _) Unit{unittype = Miner height} =
    if height == 1
    then action
    else Error "Unit does not satisfy height requirement"
validHeightCheck action@(SPLIT _) Unit{unittype = Miner height} =
    if height > 1
    then action
    else Error "Unit does not satisfy height requirement"
