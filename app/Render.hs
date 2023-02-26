module Render where

import Graphics.Gloss
import GameData

-- Change the grid color based on the game state
determineGridColor :: [(Bool, Bool)] -> Picture
determineGridColor head
  | head == [(True, True)] = pictures [color green rectangleOutline]
  | head == [(True, False)] = pictures [color green rectangleFilled]
  | head == [(False, True)] = pictures [color blue rectangleFilled]
  | otherwise = pictures [color black rectangleOutline]
   where
    rectangleOutline = rectangleWire cellWidth cellWidth
    rectangleFilled = rectangleSolid cellWidth cellWidth

-- Define the rendering function
gridPicture :: GameState -> Picture
gridPicture state = pictures
  [ -- Draw the grid
    translate (-100) (-250 + 25) $ pictures
      [ translate (fromIntegral x * cellWidth) (fromIntegral y * cellWidth) $
         determineGridColor currentHead
          , rectangleWire cellWidth cellWidth
          , translate ((-cellWidth/2) + 50 * fromIntegral x) (10 + 50 * fromIntegral y) $ scale 0.1 0.1 $ text (show (y * gridSize + x + 1))
      ]
        | x <- [0..gridSize-1], y <- [0..gridSize-1]
  ] where
      currentState = grid state
      currentHead = head currentState

-- Define the roll button picture
buttonPicture :: Picture
buttonPicture = Pictures [
    Translate (-250) 150 $ color buttonColor $ rectangleSolid 200 100,
    Translate (-250) 150 $ color blue $ rectangleWire 200 100,
    Translate (-335) 135 $ Scale 0.2 0.2 $ Text buttonText
    ]
    where
        buttonText = "Press to Roll"

-- dicePicture :: Picture
-- dicePicture = Pictures [
--      Translate (-250) 0 $ color black $ rectangleWire 50 50,
--      Translate (-260) 8 $ color black $ circleSolid 4,
--      Translate (-240) 8 $ color black $ circleSolid 4,
--      Translate (-260) (-8) $ color black $ circleSolid 4,
--      Translate (-240) (-8) $ color black $ circleSolid 4
--      ]

-- Define the "Number Rolled" picture
resultText :: Picture
resultText = Translate (-340) (-150) $ Scale 0.2 0.2 $ Text youRolled
             where youRolled = "Number Rolled:"

-- Define the "Roll Result" picture
rollResult :: Picture
rollResult = Pictures [
    Translate (-250) (-200) $ color red $ rectangleWire 80 50,
    Translate (-260) (-210) $ Scale 0.2 0.2 $ Text result
    ]
 where
     result = "2" -- TODO: to be changed to dice result (if possible)

-- Render the gameboard's components
ender :: (PlayerState, GameState) -> Picture
render (playerState, gameState) = Pictures [grid, button, result, text]
    where
        grid = gridPicture gameState
        button = buttonPicture
        result = rollResult
        text = resultText