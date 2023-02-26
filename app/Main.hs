{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant if" #-}
module Main where

{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import System.Random ( randomRIO )
import Data.List
import Constants
import GHC.IO
import GameData
import Render
import Debug.Trace

handleEvent :: Event -> (PlayerState, GameState) -> (PlayerState, GameState)
handleEvent (EventKey (MouseButton LeftButton) Down _ (x, y)) (playerState, gameState)
  | buttonClicked (round x, round y) = (playerState', gameState')
  | otherwise = (playerState, gameState)
  where 
    playerState' = unsafePerformIO $ playerAction playerState
    gameState' = updateGameState playerState' gameState
    buttonClicked (x', y') = x' > left && x' < right && y' > bottom && y' < top
    (left, right, bottom, top) = (buttonLeft, buttonRight, buttonBottom, buttonTop)
handleEvent _ (playerState, gameState) = (playerState, gameState)

playerAction :: PlayerState -> IO PlayerState
playerAction (PlayerState {turn = t, player1 = p1, player2 = p2}) = do
    rollDice <- randomRIO (1, 6)
    if t == 1
        then if (p1 + rollDice) >= 100
            then return PlayerState{turn = 2, player1 = 100, player2 = p2}
            else return PlayerState{turn = 2, player1 = p1 + rollDice, player2 = p2}
        else if t == 2
            then if (p2 + rollDice) >= 100
                then return PlayerState{turn = 1, player1 = p1, player2 = 100}
                else return PlayerState{turn = 1, player1 = p1, player2 = p2 + rollDice}
            else return PlayerState{turn = t, player1 = p1, player2 = p2}

--Updates the gamestate based on the player's state
updateGameState :: PlayerState -> GameState -> GameState
updateGameState (PlayerState {turn = t, player1 = p1, player2 = p2}) (GameState {grid = g, gameOver = o}) = 
    let p1x = div p1 10
        p1y = mod p1 10
        p2x = div p2 10
        p2y = mod p2 10
        grid = replicate gridSize $ replicate gridSize (False, False)
        updatedGrid = [[if (x, y) == (p1x, p1y) && (x, y) == (p2x, p2y) then (True, True)
                        else 
                            if (x, y) == (p2x, p2y) then (False, True)
                        else 
                            if (x, y) == (p1x, p1y) then (True, False)
                        else (False, False)
                        | y <- [0..gridSize-1]]
                        | x <- [0..gridSize-1]]
        gameOver1 = if p1 == 100 then 1 else o
        gameOver2 = if p2 == 100 then 2 else o
        gameOver' = max gameOver1 gameOver2
    in GameState{grid = updatedGrid, gameOver = gameOver'}

-- Define the update function
update :: Float -> (PlayerState, GameState) -> (PlayerState, GameState)
update _ = id

-- Define the main function
main :: IO ()
main = play
  (InWindow "Snakes and Ladders" (800, 550) (10, 10))
  white
  60
  initialGameState
  render
  handleEvent
  update