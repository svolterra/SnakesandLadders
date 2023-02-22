module GameLogic where

import System.Random


data GameState = GameState {
    turn::Int,
    player1::Int,
    player2::Int,
    gameOver::Int
}

-- For obstacles 1 means ladder, 2 means snakes
data Obstacle = Obstacle {
    direction::Int,
    startPos::Int,
    endPos::Int
}

obstacles :: [Obstacle]
obstacleList = [Obstacle 1 2 38]


initialGameState = GameState {turn = 1, player1 = 0, player2 = 0, gameOver = 0}

playerAction :: GameState -> GameState
playerAction (GameState {turn = t, player1 = p1, player2 = p2, gameover = o})
    rollDice = randomR(1, 6)
    if t == 1
        if p1 + rollDice >= 100
            then GameState{turn = 2, player1 = 100, p2, gameOver = -1}
        GameState{turn = 2, player1 = p1 + rollDice, p2, gameOver = 0}
    if t == 2
        if p2 >= 100
            then GameState{turn = 1, p1, player2 = 100, gameOver = 1}
        GameState{turn = 1, p1, player2 = p2 + rollDice, gameOver = 1}