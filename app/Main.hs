{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant if" #-}
module Main where

import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import System.Random ( randomRIO )
import Data.List
import Constants
import GHC.IO
import GameData
import Render
import Debug.Trace
import GameLogic

handleEvent :: Event -> (PlayerState, GameState) -> (PlayerState, GameState)
handleEvent (EventKey (MouseButton LeftButton) Down _ (x, y)) (playerState, gameState)
  | buttonClickedRoll (round x, round y) = (playerState', gameState')
  | buttonClickedReset (round x, round y) = resetTuple
  | otherwise = (playerState, gameState)
  where 
    playerState' = unsafePerformIO $ playerAction playerState
    gameState' = updateGameState playerState' gameState
    resetTuple = initialGameState
    buttonClickedRoll (x', y') = x' > rollLeft && x' < rollRight && y' > rollBottom && y' < rollTop
    (rollLeft, rollRight, rollBottom, rollTop) = (rollButtonLeft, rollButtonRight, rollButtonBottom, rollButtonTop)
    buttonClickedReset (x', y') = x' > resetLeft && x' < resetRight && y' > resetBottom && y' < resetop
    (resetLeft, resetRight, resetBottom, resetop) = (resetButtonLeft, resetButtonRight, resetButtonBottom, resetButtonTop)
handleEvent _ (playerState, gameState) = (playerState, gameState)

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