module Construct
    ( constructIntialGameState
    , reconstructGameState
    , constructGoldBoard
    , constructGameBoard
    , constructUnitId
    )
where

import qualified Data.Map.Strict               as Map
import           Data.Map                       ( Map() )
import           Lib
import           Flow                           ( (|>) )
import           Data.List                      ( sortOn )
import           Data.Ord                       ( comparing )


-- Construct turn queue from all units on board
-- Red and Blue units are put in separate queues in ascending order of id
constructQueue :: Map Pos Unit -> Queue
constructQueue unitboard = Queue redunits blueunits
  where
    allunits = Map.elems unitboard
    blueunits =
        allunits |> filter (\unit -> Blue == team unit) |> sortOn unitid
    redunits = allunits |> filter (\unit -> Red == team unit) |> sortOn unitid

-- construct game board creates the boundaries of the game board
constructGameBoard :: Pos -> Bool
constructGameBoard (x, y) = 0 <= x && x < 20 && 0 <= y && y < 20  -- dummy implementation

-- construct gold board populates the game board with gold mines
constructGoldBoard :: Pos -> Bool
constructGoldBoard = undefined

-- unit board maps positions on the board to units present at those positions
constructUnitBoard :: Map Pos Unit
constructUnitBoard = Map.fromList
    [(pos redbase, redbase), (pos bluebase, bluebase)]
  where
    redbase  = Unit (1, 1) 1 Red Base 1
    bluebase = Unit (19, 19) 1 Blue Base 2

-- construct initial gamestate
constructIntialGameState :: GameState
constructIntialGameState = GameState (constructQueue unitboard)
                                     (Gold 100 100)
                                     unitboard
    where unitboard = constructUnitBoard

-- reconstruct gamestate for each round by updating the turn queue
-- new turn queue is created from previous round unit board
reconstructGameState :: GameState -> GameState
reconstructGameState GameState { gamequeue = q, gamegold = gold, gameunitboard = uboard }
    = newstate
    where newstate = GameState (constructQueue uboard) gold uboard

constructUnitId :: Int
constructUnitId = undefined

nextTurnUnit :: Unit
nextTurnUnit = undefined
