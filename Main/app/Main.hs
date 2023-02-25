import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import System.Random
import Data.List
import Constants

-- Define the grid size 
gridSize :: Int
gridSize = 10

-- Define the grid cell width
cellWidth :: Float
cellWidth = 50

-- Define button colour
buttonColor :: Color
buttonColor = makeColor 0.255 0.91 0.99 1

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

-- Define the initial game state
initialGameState :: (PlayerState, GameState)
initialGameState = (
    (PlayerState {turn = 1, player1 = 0, player2 = 0}),
    GameState {
    grid = replicate gridSize $ replicate gridSize (False, False),
    gameOver = 0
    })

acc :: Int
acc = 0

determineGridColor :: [(Bool, Bool)] -> Picture
determineGridColor head
  | head == [(True, True)] = pictures [color green $ rectangleOutline, index]
  | head == [(True, False)] = pictures [color green $ rectangleFilled, index]
  | head == [(False, True)] = pictures [color blue $ rectangleFilled, index]
  | otherwise = pictures [color black $ rectangleOutline, index]
   where
    rectangleOutline = rectangleWire cellWidth cellWidth
    rectangleFilled = rectangleSolid cellWidth cellWidth
    index = Scale 0.1 0.1 $ Text (show $ (1 + acc))

-- Define the rendering function
gridPicture :: GameState -> Picture
gridPicture state = pictures
  [ -- Draw the grid
    translate (-100) (-250 + 25) $ pictures
      [ translate (fromIntegral x * cellWidth) (fromIntegral y * cellWidth) $
         determineGridColor currentHead
        | x <- [0..gridSize-1], y <- [0..gridSize-1]
      ]
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

resultText :: Picture
resultText = Translate (-340) (-150) $ Scale 0.2 0.2 $ Text youRolled
             where youRolled = "Number Rolled:"

rollResult :: Picture
rollResult = Pictures [
    Translate (-250) (-200) $ color red $ rectangleWire 80 50,
    Translate (-260) (-210) $ Scale 0.2 0.2 $ Text result
    ]
 where
     result = "2" -- TODO: to be changed to dice result (if possible)

render :: (PlayerState, GameState) -> Picture
render (playerState, gameState) = Pictures [grid, button, result, text]
    where
        grid = gridPicture gameState
        button = buttonPicture
        result = rollResult
        text = resultText

-- Define the event handling function need to rewrite this entire thing
handleEvent :: Event -> (PlayerState, GameState) -> (PlayerState, GameState)
handleEvent (EventKey (MouseButton LeftButton) Down _ (x, y)) (playerState, gameState)
  | x < 0 = (playerState, gameState) -- Clicked on the bar, do nothing
  | otherwise = (playerState, (gameState { grid = grid' }))
  where
    w = fromIntegral gridSize * cellWidth
    h = fromIntegral gridSize * cellWidth
    col = floor $ (x + w/2) / cellWidth
    row = floor $ (y + h/2) / cellWidth
    grid' = take row (grid gameState) ++ [row''] ++ drop (row+1) (grid gameState)
    row'' = take col (grid gameState !! row) ++ [(False, False)] ++ drop (col+1) (grid gameState !! row)
    playerState' = playerAction playerState

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