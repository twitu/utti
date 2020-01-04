# UTTI
Utti is a multi-player strategy game, it is unlike other games you have played. Instead of playing directly, you create a logic that takes decisions in your place. You can write the logic in your preferred programming language. Prior programming experience is not required and this game is an excellent way to dip you toes in programming.

## Overview
The game is a 2D board of 100 rows and 100 columns. Gold mines will be symmetrically distributed on the board. The board will be symmetrical vertically, horizontally or both. You and your opponent will spawn with one base on opposite sides of the line of symmetry, (vertical line if board is symmetric in both ways).

You have two types of units, the `troop` and the `base`. The base can creates troops and the troops can move to adjacent square, mine gold or create a base. Troops can interact with each other capturing or climbing on other units. Imagine troops as inverted  tumblers that can be stacked on top of each other and bases as plates (which cannot be stacked, because game logic!).

The game is played in rounds. In each round, every unit gets one action. The order of turns alternate between each players units i.e. player 1 unit takes an action, player 2 unit takes an action, player 1 unit takes an action ... and so on.

Exact rules regarding each action is given below.

## Objective
The player with the maximum score at the end 500 rounds wins the game. The formula for the score is,
> total height of troops * 5 + total number of bases * 100 + current gold reserve

## Rules and Actions

> (IDLE, (ROW, COL))

Both troop and base can choose to discard their action. It is necessary to give the row and col values for a valid input, but they are ignored.

> (CREATE_TROOP, (ROW, COL))

Only a base can create a troop. The troop must be created on a position adjacent to the base. If there is any troop standing on the base, the action will fail. The troop will take an action in the same round but will get its turn when all the existing units on the board have taken their action.

> (MINE, (ROW, COL))

A troop can mine gold if it is standing on a gold mine. The amount of gold is a function of the height of the troop i.e. `gold = height * 2`. The gold is immediately added to the player's reserves. It is necessary to give the row and col values for a valid input, but they are ignored.


> (MOVE, (ROW, COL))

Only a troop can move. The destination position, determined by the row and column values, must be adjacent to the troop's current position. The action will fail if the destination position is occupied by a troop.

> (CAPTURE, (ROW, COL))

A troop can capture an enemy troop. The destination position must be adjacent to the troop's current position. The combined troop now belongs to the player, who performed the capture. Imagine tumblers stacked on top of each other and the whole troop belonging to the tumbler on top (which performed the capture). The captured unit cannot perform any action.

> (CLIMB, (ROW, COL))

A troop can climb on a friendly troop. The destination position must be adjacent to the troop's current position. The troop that was climbed upon cannot perform any action.

> (SPLIT, (ROW, COL))

A troop can split itself in two parts if its height greater than 1. The destination position must be adjacent to the troops current position and it must not be occupied by any other unit. The top half is the old troop and the bottom half is the new troop. The old troop in moved to the destination position, while the new troop stays at the current position. The new troop can take an action in the same round after all existing units have performed their actions. The split for even and odd height is given as, `2*n (even troop) -> (n) old troop, (n) new troop` and `2*n + 1 (odd troop) -> (n) old troop, (n + 1) new troop`.
