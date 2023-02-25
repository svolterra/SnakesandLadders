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
    grid :: [[Bool]],
    gameOver::Int
  }

data PlayerState = PlayerState {
    turn::Int,
    player1::Int,
    player2::Int
}

-- Define the initial game state
initialGameState :: GameState
initialGameState = GameState {
    grid = replicate gridSize $ replicate gridSize False,
    gameOver = 0
}

-- Define the rendering function
render :: GameState -> Picture
render state = pictures
  [ -- Draw the grid
    translate (-100) (-250 + 25) $ pictures
      [ translate (fromIntegral x * cellWidth) (fromIntegral y * cellWidth) $
          if (grid state !! y) !! x
          then color black $ rectangleSolid cellWidth cellWidth
          else color black $ rectangleWire cellWidth cellWidth
      | x <- [0..gridSize-1], y <- [0..gridSize-1]
      ]
  ]
  where
    w = fromIntegral gridSize * cellWidth
    h = fromIntegral gridSize * cellWidth

-- Define the event handling function need to rewrite this entire thing
handleEvent :: Event -> GameState -> GameState
handleEvent (EventKey (MouseButton LeftButton) Down _ (x, y)) state
  | x < 0 = state -- Clicked on the bar, do nothing
  | otherwise = state { grid = grid' }
  where
    col = floor $ (x + w/2) / cellWidth
    row = floor $ (y + h/2) / cellWidth
    grid' = take row (grid state) ++ [row''] ++ drop (row+1) (grid state)
    row'' = take col (grid state !! row) ++ [not (grid state !! row !! col)] ++ drop (col+1) (grid state !! row)
    w = fromIntegral gridSize * cellWidth
    h = fromIntegral gridSize * cellWidth
handleEvent _ state = state


playerAction :: PlayerState -> IO PlayerState
playerAction (PlayerState {turn = t, player1 = p1, player2 = p2}) = do
    let gen = mkStdGen 42
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
update :: Float -> GameState -> GameState
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