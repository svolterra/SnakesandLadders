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

offsetX :: Int
offsetX = 20

offsetY :: Int
offsetY = 20

startX :: Int
startX  = 20

startY :: Int
startY  = 20

buttonLeft :: Int
buttonLeft = -250 

buttonRight :: Int
buttonRight = buttonLeft + buttonWidth

buttonBottom :: Int
buttonBottom = 150

buttonTop :: Int
buttonTop = buttonBottom + buttonHeight

buttonWidth :: Int
buttonWidth = 200

buttonHeight :: Int
buttonHeight = 100

-- Define button colour
buttonColor :: Color
buttonColor = makeColor 0.255 0.91 0.99 1