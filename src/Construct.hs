module Construct 
    ( constructInitialGameState
    , reconstructGameState
    )
where

import qualified Data.Map.Strict               as Map
import           Data.Map                       ( Map() )
import           Lib
import           Flow                           ( (|>) )
import           Data.List                      ( sortOn )
import           Data.Ord                       ( comparing )

-- constants
boardDimensions = 10

-- Construct turn queue from all units on board
-- Red and Blue units are put in separate queues in ascending order of id
constructQueue :: [Unit] -> Queue
constructQueue allunits = Queue redunits blueunits
  where
    blueunits =
        allunits |> filter (\unit -> Blue == team unit) |> sortOn unitid
    redunits = allunits |> filter (\unit -> Red == team unit) |> sortOn unitid

-- unit board maps positions on the board to units present at those positions
constructUnitBoard :: [Unit]
constructUnitBoard = [redbase, bluebase]
  where
    redbase  = Unit (1, 1) Red (Base 5) 1
    bluebase = Unit (boardDimensions - 1, boardDimensions - 1) Blue (Base 5) 2

-- construct initial gamestate
constructInitialGameState :: GameState
constructInitialGameState = GameState boardDimensions (constructQueue unitboard)
                                     (Gold 100 100)
                                     unitboard
                                     constructGoldBoard
                                     0
    where unitboard = constructUnitBoard

-- reconstruct gamestate for each round by updating the turn queue
-- new turn queue is created from previous round unit board
reconstructGameState :: GameState -> GameState
reconstructGameState state = newstate
    where newstate = state {queue = constructQueue $ gameunits state}

-- construct gold board populates the game board with gold mines
constructGoldBoard :: [Bool]
constructGoldBoard = [False,False,False,True,False,False,False,False,True,False,False,False,False,False,False,False,False,False,False,True,False,False,False,True,False,False,False,False,False,True,True,False,True,False,False,True,False,True,False,False,False,False,False,False,False,False,False,True,False,False,False,False,False,True,False,False,True,True,False,False,False,False,False,False,False,True,True,False,True,False,False,False,False,True,True,True,False,False,True,True,True,False,False,False,False,False,True,True,False,False,False,True,True,False,False,False,False,True,False,False] 