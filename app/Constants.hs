module Constants where

import Graphics.Gloss

-- Define the screen width of the game window
screenWidth :: Int 
screenWidth = 800

-- Define the screen height of the game window
screenHeight :: Int
screenHeight = 550

-- Define the grid size 
gridSize :: Int
gridSize = 10

-- Define the grid cell width
cellWidth :: Float
cellWidth = 50

--RollButton
rollButtonLeft :: Int
rollButtonLeft = -350
-- Define solid-color rectangles
filledSquare :: Picture
filledSquare = rectangleSolid cellWidth cellWidth

-- Define outline-only rectangles
outlinedSquare :: Picture 
outlinedSquare = rectangleWire cellWidth cellWidth

buttonLeft :: Int
buttonLeft = -350

rollButtonRight :: Int
rollButtonRight = rollButtonLeft + buttonWidth

rollButtonBottom :: Int
rollButtonBottom = 100

rollButtonTop :: Int
rollButtonTop = rollButtonBottom + buttonHeight

--ResetButton
resetButtonLeft :: Int
resetButtonLeft = -350

resetButtonRight :: Int
resetButtonRight = resetButtonLeft + buttonWidth

resetButtonBottom :: Int
resetButtonBottom = -75

resetButtonTop :: Int
resetButtonTop = resetButtonBottom + buttonHeight

buttonWidth :: Int
buttonWidth = 200

buttonHeight :: Int
buttonHeight = 100

buttonText :: String
buttonText = "Press to Roll"

backgroundColor :: Color
backgroundColor = white

playerOneColor :: Color
playerOneColor = green

playerTwoColor :: Color
playerTwoColor = blue