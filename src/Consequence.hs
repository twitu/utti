module Consequence
    ( applyConsequenceFunc
    )
where

import           Lib
import           Construct
import           System.IO
import           Control.Monad.Writer

-- defining constants
newUnitHeight = 1
unitCaptureHeight = 1
mineGoldAmount = 5
unitGoldCost = 10
unitMaxHeight = 20
baseMaxHealth = 5
destroyBaseReward = 200

-- change functions modify game state and return a new copy

-- change game gold with given amount and log change
changeGold :: Gold -> GameState -> Writer [String] GameState
changeGold amount state@GameState{gold = gold}
    = writer (state {gold = newgold}, ["changeGold " ++ show newgold])
  where
    newgold = Gold ((redgold amount +) $ redgold gold)
                   ((bluegold amount +) $ bluegold gold)

-- remove unit at given position from unit board and log change
removeUnit :: Pos -> GameState -> Writer [String] GameState
removeUnit target state@GameState{gameunits = units}
    = writer
        (state{gameunits=remunit}, ["removeUnit from pos " ++ show target])
    where remunit = filter (\unit -> pos unit /= target) units

-- add unit at given position to unit board and log change
addUnit :: Unit -> GameState -> Writer [String] GameState
addUnit unit state@GameState{gameunits = units}
    = writer
        (state{gameunits = newunits}, ["addUnit " ++ show unit])
    where newunits = unit : units

-- add unit to its team queue and log change
addToQueue :: Unit -> GameState -> Writer [String] GameState
addToQueue unit@Unit{team = Red} state@GameState{queue = Queue redque blueque}
    = writer (state{queue = newqueue}, ["addToQueue Red " ++ show unit])
    where
        newredqueue = redque ++ [unit]
        newqueue = Queue newredqueue blueque
addToQueue unit@Unit{team = Blue} state@GameState{queue = Queue redque blueque}
    = writer (state{queue = newqueue}, ["addToQueue Blue " ++ show unit])
    where
        newbluequeue = blueque ++ [unit]
        newqueue = Queue redque newbluequeue

-- remove unit from queue and log change
removeFromQueue :: Pos -> GameState -> Writer [String] GameState
removeFromQueue removepos state@GameState{queue = queue}
    = writer (state{queue = newqueue}, ["removeFromQueue unit at position " ++ show removepos])
    where
        bluenew = filter (\unit -> removepos /= pos unit) $ bluequeue queue
        rednew = filter (\unit -> removepos /= pos unit) $ redqueue queue
        newqueue = Queue rednew bluenew

-- consequence functions make changes to game state and log them for an action

-- create action adds a unit to the board and queue
createConsequence :: Unit -> Action -> GameState -> Writer [String] GameState
createConsequence unit (CREATE pos) state@GameState{constructid = id}
    = changeGold updategold state >>= addUnit newUnit >>= addToQueue newUnit
  where
    unitteam = team unit
    updategold = if unitteam == Red then Gold (-unitGoldCost) 0 else Gold 0 (-unitGoldCost)
    newUnit    = Unit pos unitteam (Miner newUnitHeight) id
    updatestate = state {constructid = id + 1}

-- move action moves a unit
moveConsequence :: Unit -> Action -> GameState -> Writer [String] GameState
moveConsequence unit (MOVE nextpos) state =
    removeUnit curpos state >>= addUnit movedunit
  where
    curpos = pos unit
    movedunit = unit {pos = nextpos}

-- capture action creates a new unit by stacking captured and captor unit
captureConsequence :: Unit -> Action -> GameState -> Writer [String] GameState
captureConsequence unit (CAPTURE capturepos) state
    = removeUnit curpos state
        >>= removeUnit capturepos
        >>= addUnit newunit
        >>= removeFromQueue capturepos
  where
    curpos = pos unit
    oldunit = head $ filter (\unit -> pos unit == capturepos) $ gameunits state
    Miner oldheight = unittype oldunit
    newheight = oldheight + 1
    newunit = if newheight == unitMaxHeight
            then unit {pos = capturepos, unittype = Base baseMaxHealth}
            else unit {pos = capturepos, unittype = Miner newheight}

-- mine action mines gold in proportion to height of unit
mineConsequence :: Unit -> Action -> GameState -> Writer [String] GameState
mineConsequence Unit {team = unitteam, unittype = Miner height} _ = changeGold updategold
  where
    updategold =
        if unitteam == Red then Gold (mineGoldAmount * height) 0 else Gold 0 (mineGoldAmount * height)

-- split action splits a unit in half and adds the newunit to game queue
splitConsequnce :: Unit -> Action -> GameState -> Writer [String] GameState
splitConsequnce unit@Unit{pos = curpos, unittype = Miner height, team = unitteam, unitid = curid} (SPLIT splitpos) state@GameState{constructid = id}
    = removeUnit curpos newstate
        >>= addUnit oldunit
        >>= addUnit newunit
        >>= addToQueue newunit
  where
    halfheight = height `div` 2
    newunit = Unit splitpos unitteam (Miner (height - halfheight)) (id + 1)
    oldunit = Unit curpos unitteam (Miner halfheight) curid
    newstate = state {constructid = id + 1}

-- TODO: attacking a base pushes its turn to the end of the queue
-- fix by doing an in place update in the queue
-- attack action removes one health from the attacked base
-- if the health reaches 0, the base is destroyed
attackConsequence :: Unit -> Action -> GameState -> Writer [String] GameState
attackConsequence Unit {team = unitteam} (ATTACK attackpos) state =
    if curhealth == 1
        then
            removeUnit attackpos state
            >>= changeGold updateGold
        else
            removeUnit attackpos state
            >>= addUnit updatebase
            >>= removeFromQueue attackpos
            >>= addToQueue updatebase
    where
        enemybase = head $ filter (\unit -> pos unit == attackpos) $ gameunits state
        Base curhealth = unittype enemybase
        updatebase = enemybase {unittype = Base $ curhealth - 1}
        updateGold = if unitteam == Red then Gold destroyBaseReward 0 else Gold 0 destroyBaseReward

-- chooses which consequence to apply based on action
applyConsequenceFunc :: Unit -> GameState -> Action -> (GameState, [String])
applyConsequenceFunc unit state action@IDLE =
    (state, [])
applyConsequenceFunc unit state action@(CREATE _) =
    runWriter $ createConsequence unit action state
applyConsequenceFunc unit state action@MINE =
    runWriter $ mineConsequence unit action state
applyConsequenceFunc unit state action@(MOVE _) =
    runWriter $ moveConsequence unit action state
applyConsequenceFunc unit state action@(CAPTURE _) =
    runWriter $ captureConsequence unit action state
applyConsequenceFunc unit state action@(SPLIT _) =
    runWriter $ splitConsequnce unit action state
applyConsequenceFunc unit state action@(ATTACK _) =
    runWriter $ attackConsequence unit action state
applyConsequenceFunc unit state action@(Error message) =
    (state, [message])
