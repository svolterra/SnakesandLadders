{-# LANGUAGE FlexibleContexts #-}
module Render where

import qualified Data.Map as Map
import Graphics.Gloss
import Constants
import GameData

-- Determine if the game has just started
-- Return true if yes, false otherwise
isGameStart :: PlayerState -> Bool
isGameStart state = p1 == 0 && p2 == 0
                    where
                    p1 = player1 state
                    p2 = player2 state

-- Change the first cell color to red at the beginning of the game (indicates both players present)
changeIfStart :: PlayerState -> Picture
changeIfStart playerState
  | isGameStart playerState = color red filledSquare
  | otherwise = color black outlinedSquare
  where
    p1 = player1 playerState
    p2 = player2 playerState

-- Change the cell color based on the game state
updateCellColor :: (Bool, Bool) -> Picture
updateCellColor cell
  | cell == (True, True) = color red filledSquare
  | cell == (True, False) = color playerOneColor filledSquare
  | cell == (False, True) = color playerTwoColor filledSquare
  | cell == (False, False) = color lightBlack outlinedSquare
  | otherwise = color red outlinedSquare -- Render a red outlined grid if none of the conditions above are satisfied

-- Add snakes and ladders to the grid
addObstacles :: (Int, Int) -> Picture
addObstacles (x, y) =
  case Map.lookup coordsToInt obstacleDict of
    Just exit -> if coordsToInt < exit
                    then color laddersColor $ translate  (8 + 50 * fromIntegral x) (10 + 50 * fromIntegral y) $ scale 0.1 0.1 $ text (show (exit + 1))
                    else color snakesColor $ translate  (8 + 50 * fromIntegral x) (10 + 50 * fromIntegral y) $ scale 0.1 0.1 $ text (show (exit + 1))
    Nothing -> Blank
  where
    coordsToInt = y * 10 + x

-- Define the game board grid
gridPicture :: GameState -> PlayerState -> Picture
gridPicture gameState playerState = pictures
  [ -- Draw the grid
    translate (-100) (-250 + 25) $ pictures
      [ translate (fromIntegral x * cellWidth) (fromIntegral y * cellWidth) $
            updateCellColor (currentState !! y !! x)
          , changeIfStart playerState
          , addObstacles (x,y)
          , translate ((-cellWidth/2) + 50 * fromIntegral x) (10 + 50 * fromIntegral y) $ scale 0.1 0.1 $ text (show (y * gridSize + x + 1))
      ]
        | x <- [0..gridSize-1], y <- [0..gridSize-1]
  ]
  where
      currentState = grid gameState

-- Adjust button color to match current player 
changeButtonColor :: PlayerState -> Color
changeButtonColor playerState
  | turn playerState  == 1 = playerOneColor
  | otherwise = playerTwoColor

-- Define the roll button picture
rollButtonPicture :: PlayerState -> Picture
rollButtonPicture state = Pictures [
    Translate (-250) 150 $ color (changeButtonColor state) $ rectangleSolid  (fromIntegral rollButtonWidth) (fromIntegral rollButtonHeight),
    Translate (-328) 138 $ Scale 0.18 0.18 $ Text rollButtonText
    ]

-- Define the reset button picture
resetButtonPicture :: Picture
resetButtonPicture = Pictures [
    Translate (-250) 40 $ Color resetButtonColor $ rectangleSolid (fromIntegral resetButtonWidth) (fromIntegral resetButtonHeight),
    Translate (-275) 35 $ Scale 0.15 0.15 $ Text resetButtonText
  ]

-- Define the "Number Rolled" screen text
rollResultText :: Picture
rollResultText = Translate (-340) (-150) $ Scale 0.2 0.2 $ Text youRolled
             where youRolled = "Number Rolled:"

-- Adjust resut box outline to display who rolled which color
changeRollResultBoxOutline :: PlayerState -> Color
changeRollResultBoxOutline playerState
  | isGameStart playerState = white
  | turn playerState  == 1 = playerTwoColor
  | otherwise = playerOneColor
  where
    p1 = player1 playerState
    p2 = player2 playerState

-- Define the box representing the rolled number
rollResultBox :: PlayerState -> Picture
rollResultBox playerState = Pictures [
    Translate (-250) (-200) $ color (changeRollResultBoxOutline playerState) $ rectangleSolid 80 50,
    Translate (-260) (-210) $ Scale 0.2 0.2 $ color black $ Text result
    ]
 where
     result = show (diceRoll playerState)

-- Determine if the game is over 
isGameOver :: GameState -> Bool
isGameOver gameState = winner == 1 || winner == 2
                       where winner = gameOver gameState

-- Render winning screen with name and color of winner
renderWinningScreen :: GameState -> Picture
renderWinningScreen state
  | winner == 1 = color playerOneColor $ rectangleSolid (fromIntegral screenWidth) (fromIntegral screenHeight)
  | otherwise = color playerTwoColor $ rectangleSolid (fromIntegral screenWidth) (fromIntegral screenHeight)
  where
  winner = gameOver state

-- Render text mentioning the game winner
declareWinner :: GameState -> Picture
declareWinner state
  | winner == 1 = Translate (-50) 25 $ Scale 0.3 0.3 $ Text "Green Wins!"
  | otherwise = Translate (-50) 25 $ Scale 0.3 0.3  $ Text "Blue Wins!"
  where
    winner = gameOver state
    
-- Render the gameboard's components
render :: (PlayerState, GameState) -> Picture
render (playerState, gameState)
    | isGameOver gameState = Pictures [renderWinningScreen gameState, winnerText, resetButton]
    | otherwise = Pictures [grid, rollButton, result, resultText, resetButton]
    where
        grid = gridPicture gameState playerState
        rollButton = rollButtonPicture playerState
        resetButton = resetButtonPicture
        result = rollResultBox playerState
        resultText = rollResultText
        winnerText = declareWinner gameState