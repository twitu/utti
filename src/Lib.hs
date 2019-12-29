module Lib
    ( Team(..)
    , Type(..)
    , Pos
    , Height
    , Unit(..)
    , ActionName(..)
    , Action(..)
    , Gold(..)
    , Queue(..)
    , GameState(..)
    )
where

import qualified Data.Map.Strict               as Map
import           Data.Map                       ( Map() )
import           Data.Maybe

data Team = Red | Blue deriving (Show, Read, Eq, Enum)
data Type = Troop | Base deriving (Show, Read, Eq, Enum)
type Pos = (Int, Int)
type Height = Int
data Unit = Unit {pos :: Pos, height :: Height, team :: Team, unittype :: Type, unitid :: Int} deriving (Show, Read)

data Gold = Gold { redgold :: Int, bluegold :: Int}  deriving (Show) -- indicating amount of gold for two players Red and Blue respectively
data Queue = Queue { redqueue :: [Unit], bluequeue :: [Unit]}

data ActionName = IDLE | CREATE_TROOP | MINE | MOVE | CAPTURE | CLIMB | SPLIT deriving (Show, Read, Eq, Enum)
data Action = Action {actionname :: ActionName, actionpos :: Maybe Pos, actionunit :: Unit} deriving (Read)
data GameState = GameState {gamequeue :: Queue, gamegold :: Gold, gameunitboard :: Map Pos Unit}
data Move = Move {movename :: ActionName, movepos :: Maybe Pos}
