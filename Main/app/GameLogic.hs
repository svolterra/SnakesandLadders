module GameLogic where

import System.Random

-- For turn:
--     1 = player 1's turn
--     2 = player 2's turn
-- For gameOver:
--     0 = game continues
--     1 = player 1 wins
--     2 = player 2 wins
data GameState = GameState {
    turn::Int,
    player1::Int,
    player2::Int,
    gameOver::Int
} deriving(Show, Eq)

-- For obstacles 1 means ladder, 2 means snakes
data Obstacle = Obstacle {
    direction::Int,
    startPos::Int,
    endPos::Int
} deriving (Eq, Show)

obstacleList :: [Obstacle]
obstacleList = [Obstacle 1 2 38]


initialGameState :: GameState
initialGameState = GameState {turn = 1, player1 = 0, player2 = 0, gameOver = 0}

playerAction :: GameState -> IO GameState
playerAction (GameState {turn = t, player1 = p1, player2 = p2, gameOver = o}) = do
    let gen = mkStdGen 42
    rollDice <- randomRIO (1, 6)
    if t == 1
        then if (p1 + rollDice) >= 100
            then return GameState{turn = 2, player1 = 100, player2 = p2, gameOver = -1}
            else return GameState{turn = 2, player1 = p1 + rollDice, player2 = p2, gameOver = 0}
        else if t == 2
            then if (p2 + rollDice) >= 100
                then return GameState{turn = 1, player1 = p1, player2 = 100, gameOver = 1}
                else return GameState{turn = 1, player1 = p1, player2 = p2 + rollDice, gameOver = 1}
            else return GameState{turn = t, player1 = p1, player2 = p2, gameOver = o}