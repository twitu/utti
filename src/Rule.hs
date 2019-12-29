module Rule
    ( goldAvailable
    , isEnemyOnPos
    , isFriendlyOnPos
    , unitOnGold
    , posIsOccupied
    , posIsNotOccupied
    , posIsOccupiedByTroop
    , posAdjacentToUnit
    , unitIsBase
    , unitIsTroop
    , validHeight
    , validPos
    )
where

import           Lib
import qualified Data.Map.Strict               as Map
import           Data.Map                       ( Map() )
import           Data.Maybe
import           Construct

-- check if unit performing action is of type base
unitIsBase :: Action -> Maybe Action
unitIsBase action@Action { actionunit = Unit { unittype = Base } } =
    Just action
unitIsBase _ = Nothing

-- check if unit performing action is of type troop
unitIsTroop :: Action -> Maybe Action
unitIsTroop action@Action { actionunit = Unit { unittype = Troop } } =
    Just action
unitIsTroop _ = Nothing

-- check if position where action will be performed is valid
validPos :: Action -> Maybe Action
validPos action@Action { actionpos = Just (row, col) } =
    if 0 <= row && row < 20 && 0 <= col && col < 20  -- board limits
        then Just action
        else Nothing
validPos _ = Nothing

-- checks if position is occupied by any unit
posIsOccupied :: GameState -> Action -> Maybe Action
posIsOccupied GameState { gameunitboard = unitboard } action@(Action _ (Just pos) _)
    = if isJust . Map.lookup pos $ unitboard then Just action else Nothing
posIsOccupied _ _ = Nothing

-- checks if position is not occupied by any unit
posIsNotOccupied :: GameState -> Action -> Maybe Action
posIsNotOccupied GameState { gameunitboard = unitboard } action@(Action _ (Just pos) _)
    = if isNothing . Map.lookup pos $ unitboard then Just action else Nothing
posIsNotOccupied _ _ = Nothing

-- checks if position is occupied by a troop
-- Note: should only be called after posIsOccupied; will throw runtime error if pos is unoccupied
posIsOccupiedByTroop :: GameState -> Action -> Maybe Action
posIsOccupiedByTroop GameState { gameunitboard = uboard } action@(Action _ (Just pos) _)
    = if unittype posunit == Troop then Just action else Nothing
    where posunit = (Map.!) uboard pos
posIsOccupiedByTroop _ _ = Nothing

-- checks if unit performing action is on a gold mine
unitOnGold :: GameState -> Action -> Maybe Action
unitOnGold state action@Action { actionunit = unit } =
    if constructGoldBoard $ pos unit then Just action else Nothing

-- checks if pos where action will be performed is adjacent to unit performing action
posAdjacentToUnit :: Action -> Maybe Action
posAdjacentToUnit action@Action { actionunit = Unit { pos = (row, col) }, actionpos = Just adjpos }
    = if adjpos `elem` adjposlist then Just action else Nothing
  where
    adjposlist =
        [(row - 1, col), (row + 1, col), (row, col - 1), (row, col + 1)]  -- movement limits
posAdjacentToUnit _ = Nothing

-- check if unit where action will be performed is friendly
-- Note: should only be called after posIsOccupied; will throw runtime error if pos is unoccupied
isFriendlyOnPos :: GameState -> Action -> Maybe Action
isFriendlyOnPos GameState { gameunitboard = uboard } action@(Action _ (Just pos) Unit { team = curteam })
    = if team posunit == curteam then Just action else Nothing
    where posunit = (Map.!) uboard pos
isFriendOnPos _ _ = Nothing

-- check if unit where action will be performed is enemy
-- Note: should only be called after posIsOccupied; will throw runtime error if pos is unoccupied
isEnemyOnPos :: GameState -> Action -> Maybe Action
isEnemyOnPos GameState { gameunitboard = uboard } action@(Action _ (Just pos) Unit { team = curteam })
    = if team posunit /= curteam then Just action else Nothing
    where posunit = (Map.!) uboard pos
isEnemyOnPos _ _ = Nothing

-- check different levels of gold availability based on action
goldAvailable :: GameState -> Action -> Maybe Action
goldAvailable GameState { gamegold = amount } action@(Action CREATE_TROOP _ Unit { team = Red })
    = if redgold amount > 10 then Just action else Nothing
goldAvailable GameState { gamegold = amount } action@(Action CREATE_TROOP _ Unit { team = Blue })
    = if bluegold amount > 10 then Just action else Nothing
goldAvailable _ _ = Nothing

-- checks if unit has valid height to perform action
validHeight :: GameState -> Action -> Maybe Action
validHeight _ action@(Action SPLIT _ unit) =
    if height unit > 1 then Just action else Nothing
validHeight _ _ = Nothing
