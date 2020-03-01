# UTTI

Utti is a multi-player strategy game, it is unlike other games you have played. Instead of playing directly, you create a logic that takes decisions in your place. You can write the logic in your preferred programming language. Prior programming experience is not required and this game is an excellent way to dip you toes in programming.

## Overview

The game is a 2D board of 100 rows and 100 columns. Gold mines will be symmetrically distributed on the board. The board will be symmetrical vertically, horizontally or both. You and your opponent will spawn with one base on opposite sides of the line of symmetry, (vertical line if board is symmetric in both ways).

You have two types of units, the `miner` and the `base`. The base can creates miners and the miners can move to adjacent square, mine gold or create a base. Troops can interact with each other capturing or climbing on other units. Imagine miners as cones that can be stacked on top of each other and bases as objects (which cannot be stacked, because game logic!).

The game is played in rounds. In each round, every unit gets one action. The order of turns alternate between each players units i.e. player 1 unit takes an action, player 2 unit takes an action, player 1 unit takes an action ... and so on.

Exact rules regarding each action is given below.

## Objective

The player with the maximum score at the end 500 rounds wins the game. The formula for the score is,

> total height of miners _ 5 + total number of bases _ 100 + current gold reserve

## Rules and Actions

> IDLE

Both miner and base can choose to discard their action. It is necessary to give the row and col values for a valid input, but they are ignored.

> CREATE (ROW, COL)

Only a base can create a miner. The miner must be created on a position adjacent to the base. If there is any miner standing on the base, the action will fail. The miner will take an action in the same round but will get its turn when all the existing units on the board have taken their action.

> MINE (ROW, COL)

A miner can mine gold if it is standing on a gold mine. The amount of gold is a function of the height of the miner i.e. `gold = height * 5`. The gold is immediately added to the player's reserves. It is necessary to give the row and col values for a valid input, but they are ignored.

> MOVE (ROW, COL)

Only a miner can move. The destination position, determined by the row and column values, must be adjacent to the miner's current position. The action will fail if the destination position is occupied by a miner.

> CAPTURE (ROW, COL)

A miner can capture another miner, friendly or enemy. The destination position must be adjacent to the miner's current position. The combined miner now belongs to the player, who performed the capture. Imagine cones stacked on top of each other and the whole stack belonging to the tumbler on top (which performed the capture).

The new unit created has exhausted its action for that turn. The miner performing the capture must be of height 1. The new miner has one height greater than the capture miner.

Note: If upon capture the new miner reaches a height of 20, it is automatically converted to a Base of health 5. This change is permanent since a Base cannot split.

> SPLIT (ROW, COL)

A miner can split itself in two parts if its height greater than 1. The destination position must be adjacent to the miner's current position and it must not be occupied by any other unit.

Imagine the stack of cones being split in half. The top half is the old miner and the bottom half is the new miner. The new miner in moved to the destination position, while the old miner stays at the current position. The new miner is added to the turn queue and can take an action in the same round after all existing units have performed their actions. The split for even and odd height is given as, `2*n (even miner) -> (n) old miner, (n) new miner` and `2*n + 1 (odd miner) -> (n) old miner, (n + 1) new miner`.

> SPLIT (ROW, COL)

A miner can split itself in two parts if its height greater than 1. The destination position must be adjacent to the miner's current position and it must not be occupied by any other unit.

Imagine the stack of cones being split in half. The top half is the old miner and the bottom half is the new miner. The new miner in moved to the destination position, while the old miner stays at the current position. The new miner is added to the the queue and can take an action in the same round after all existing units have performed their actions. The split for even and odd height is given as, `2*n (even miner) -> (n) old miner, (n) new miner` and `2*n + 1 (odd miner) -> (n) old miner, (n + 1) new miner`.

> ATTACK (ROW, COL)

A miner (of any height) can attack an enemy base reducing its health. Performing an attack consumes the miners action for that turn. If the attack reduces the Base health to 0, the enemy base is destroyed and the attacking team receives 200 gold as reward.

## Playing the game

It is assumed that you have stack and git installed, if not refer to the [stack guide](https://docs.haskellstack.org/en/stable/README/) and [git guide](https://git-scm.com/book/en/v2/Getting-Started-Installing-Git). Then follow these steps

```
git clone https://github.com/twitu/utti.git
cd utti
stack run
```

The game will print out helpful information and ask for you input as to what action to perform. The format for action is exactly has one given the rules section. For example, to move the current unit to position (6, 7), you need to type `MOVE (6, 7)`.

Note: This game is still in development, bugs are highly likely, please keep calm and report them.
