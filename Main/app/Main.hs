import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import Constants
import System.Random
-- Define the grid size and cell width
gridSize :: Int
gridSize = 10

cellWidth :: Float
cellWidth = 50

-- Define the game state data type
data GameState = GameState{
    grid :: [[(Bool, Bool)]],
    gameOver::Int
  }

data PlayerState = PlayerState {
    turn::Int,
    player1::Int,
    player2::Int
}

-- Define the initial game state
initialGameState :: (PlayerState, GameState)
initialGameState = ((PlayerState {turn = 1, player1 = 0, player2 = 0}),
    GameState {
    grid = replicate gridSize $ replicate gridSize (False, False),
    gameOver = 0
    })

-- Define the rendering function
gridPicture :: GameState -> Picture
gridPicture state = pictures
  [ -- Draw the grid
    translate (-100) (-250 + 25) $ pictures
      [ translate (fromIntegral x * cellWidth) (fromIntegral y * cellWidth) $
        color black $ rectangleWire cellWidth cellWidth
        | x <- [0..gridSize-1], y <- [0..gridSize-1]
      ]
  ]
  where
    w = fromIntegral gridSize * cellWidth
    h = fromIntegral gridSize * cellWidth

buttonPicture :: Picture
buttonPicture = Pictures [
    Translate (-250) 150 $ Color black $ rectangleWire 200 100,
    Translate (-335) 135 $ Scale 0.2 0.2 $ Text buttonText
    ]
    where
        buttonText = "Press to Roll"

render :: (PlayerState, GameState) -> Picture
render (pState, gState) = Pictures [grid, button]
    where
        grid = gridPicture gState
        button = buttonPicture

-- Define the event handling function need to rewrite this entire thing
handleEvent :: Event -> (PlayerState, GameState) -> (PlayerState, GameState)
handleEvent (EventKey (MouseButton LeftButton) Down _ (x, y)) (pState, gState)
  | x < 0 = (pState, gState) -- Clicked on the bar, do nothing
  | otherwise = (pState, (gState { grid = grid' }))
  where
    col = floor $ (x + w/2) / cellWidth
    row = floor $ (y + h/2) / cellWidth
    grid' = take row (grid gState) ++ [row''] ++ drop (row+1) (grid gState)
    row'' = take col (grid gState !! row) ++ [(False, False)] ++ drop (col+1) (grid gState !! row)
    pState' = playerAction pState
    w = fromIntegral gridSize * cellWidth
    h = fromIntegral gridSize * cellWidth
handleEvent _ (pState, gState) = (pState, gState)


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
  (InWindow "Grid with Bar" (800, 550) (10, 10))
  white
  60
  initialGameState
  render
  handleEvent
  update