module Lib
    ( Team(..)
    , Type(..)
    , Pos
    , Height
    , Unit(..)
    , Action(..)
    , Gold(..)
    , Queue(..)
    , GameState(..)
    )
where

import           Data.Maybe

data Team = Red | Blue deriving (Show, Read, Eq, Enum)
data Type = Miner {height :: Int} | Base {health :: Int} deriving (Show, Read, Eq)
type Pos = (Int, Int)
type Height = Int
data Unit = Unit {pos :: Pos, team :: Team, unittype :: Type, unitid :: Int} deriving (Show, Read, Eq)

data Gold = Gold {redgold :: Int, bluegold :: Int}  deriving (Show) -- indicating amount of gold for two players Red and Blue respectively
data Queue = Queue {redqueue :: [Unit], bluequeue :: [Unit]} deriving (Show)

data Action = IDLE | MINE | CREATE Pos | MOVE Pos | CAPTURE Pos | SPLIT Pos | ATTACK Pos | Error String deriving (Show, Read, Eq)
data GameState = GameState {bound :: Int, queue :: Queue, gold :: Gold, gameunits :: [Unit], goldMine :: [Bool], constructid :: Int} deriving (Show)
