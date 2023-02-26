module GameData where

import Constants

-- Define the game state data type
-- grid stores pairs of Booleans representing each player's presence at a certain cell
-- gameOver is 1 if the game is over or 0 if the game continues
data GameState = GameState {
    grid :: [[(Bool, Bool)]],
    gameOver::Int
}

-- Define the player state data type 
data PlayerState = PlayerState {
    turn :: Int,
    player1 :: Int,
    player2 :: Int
}