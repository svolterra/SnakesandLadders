module GameData where
import qualified Data.Map as Map
import Constants
import qualified Control.Applicative as Map

-- Define the game state data type
-- grid stores pairs of Booleans representing each player's presence at a certain cell
-- gameOver is 1 if the game is over and player 1 won
--             2 if the game is over and player 2 won
--          or 0 if the game continues
data GameState = GameState {
    grid :: [[(Bool, Bool)]],
    gameOver::Int
} deriving (Eq, Show)

-- Define the player state data type
data PlayerState = PlayerState {
    turn :: Int,
    player1 :: Int,
    player2 :: Int
} deriving (Eq, Show)

-- Insert stuff into the map
-- The key is the start of the snake / ladder
-- The Value is the exit
-- RULE: Only assign one value per key
obstacleList :: [(Int, Int)]
obstacleList = [(2, 10), (15, 20), (36, 48), (72, 88), (25, 10), (42, 30), (72, 40), (99, 12)]

-- Initialize the map that has nothing there
obstacleDict :: Map.Map Int Int
obstacleDict = Map.fromList obstacleList

-- Define the initial game state
initialGameState :: (PlayerState, GameState)
initialGameState = (
    (PlayerState {turn = 1, player1 = 0, player2 = 0}),
    GameState {
    grid = replicate gridSize $ replicate gridSize (False, False),
    gameOver = 0
})