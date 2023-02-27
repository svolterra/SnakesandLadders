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

--RollButton
rollButtonLeft :: Int
rollButtonLeft = -350

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

-- Define button colour
buttonColor :: Color
buttonColor = green

buttonText :: String
buttonText = "Press to Roll"