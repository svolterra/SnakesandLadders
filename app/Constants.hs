module Constants where

import Graphics.Gloss

screenWidth :: Int 
screenWidth = 800

screenHeight :: Int
screenHeight = 550

-- Define the grid size 
gridSize :: Int
gridSize = 10

-- Define the grid cell width
cellWidth :: Float
cellWidth = 50

buttonLeft :: Int
buttonLeft = -350

buttonRight :: Int
buttonRight = buttonLeft + buttonWidth

buttonBottom :: Int
buttonBottom = 100

buttonTop :: Int
buttonTop = buttonBottom + buttonHeight

buttonWidth :: Int
buttonWidth = 200

buttonHeight :: Int
buttonHeight = 100

-- Define button colour
buttonColor :: Color
buttonColor = green

buttonText :: String
buttonText = "Press to Roll"