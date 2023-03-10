module Constants where

import Graphics.Gloss
import Graphics.Gloss.Data.Color

-- Define the screen width of the game window
screenWidth :: Int 
screenWidth = 800

-- Define the screen height of the game window
screenHeight :: Int
screenHeight = 550

-- Define the background color of the game window screen 
screenBackgroundColor :: Color
screenBackgroundColor = makeColorI 250 240 230 0

-- Define the grid size 
gridSize :: Int
gridSize = 10

-- Define the grid cell width
cellWidth :: Float
cellWidth = 50

-- Define solid-color rectangles
filledSquare :: Picture
filledSquare = rectangleSolid cellWidth cellWidth

-- Define outline-only rectangles
outlinedSquare :: Picture 
outlinedSquare = rectangleWire cellWidth cellWidth

-- RollButton
rollButtonWidth :: Int
rollButtonWidth = 200

rollButtonHeight :: Int
rollButtonHeight = 100

rollButtonLeft :: Int
rollButtonLeft = -350

rollButtonRight :: Int
rollButtonRight = rollButtonLeft + rollButtonWidth

rollButtonBottom :: Int
rollButtonBottom = 100

rollButtonTop :: Int
rollButtonTop = rollButtonBottom + rollButtonHeight

-- The text displayed in the roll button
rollButtonText :: String
rollButtonText = "Press to Roll"

--ResetButton
resetButtonWidth :: Int
resetButtonWidth = 100

resetButtonHeight :: Int
resetButtonHeight = 50

resetButtonLeft :: Int
resetButtonLeft = -300

resetButtonRight :: Int
resetButtonRight = resetButtonLeft + resetButtonWidth

resetButtonBottom :: Int
resetButtonBottom = 15

resetButtonTop :: Int
resetButtonTop = resetButtonBottom + resetButtonHeight

-- The text displayed in the reset button
resetButtonText :: String
resetButtonText = "Reset"

-- The color of the reset button
resetButtonColor :: Color
resetButtonColor = light(light (dim red))

-- Color representing ladders on the board
laddersColor :: Color
laddersColor = dark(dark yellow)

-- Color representing snakes on the board
snakesColor :: Color
snakesColor = dark red

-- Color representing player one
playerOneColor :: Color
playerOneColor = dark green

-- Color representing player two
playerTwoColor :: Color
playerTwoColor = dark cyan

-- Define a light black color for the grid
lightBlack :: Color
lightBlack = light(light black)

-- Insert stuff into the map
-- The key is the start of the snake / ladder
-- The Value is the exit
-- RULE: Only assign one value per key
-- NOTE: The assigned values for each will be kind of weird, because the list starts at 0, all
--      values will be shifted to the right by 1, keep that in mind
obstacleList :: [(Int, Int)]
obstacleList = [(1, 10), (9, 4), (15, 25), (21, 11), (27, 45), (34, 20), (41, 61), (51, 31), (57, 77), (68, 55), (73, 93), (88, 95), (99, 16)]