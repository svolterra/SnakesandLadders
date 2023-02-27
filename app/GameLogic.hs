{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant if" #-}
module GameLogic where

import Data.Maybe
import qualified Data.Map as Map
import System.Random
import GameData
import Constants

playerAction :: PlayerState -> IO PlayerState
playerAction (PlayerState {turn = t, player1 = p1, player2 = p2}) = do
    rollDice <- randomRIO (1, 6)
    if t == 1
        then if (p1 + rollDice) >= 100
            then return PlayerState{turn = 2, player1 = 100, player2 = p2}
                else case Map.lookup (p1 + rollDice) obstacleDict of
                    Just exit -> return PlayerState{turn = 2, player1 = (fromMaybe (p1 + rollDice) (Map.lookup (p1 + rollDice) obstacleDict)), player2 = p2}
                    Nothing -> return PlayerState{turn = 2, player1 = p1 + rollDice, player2 = p2}
        else if t == 2
            then if (p2 + rollDice) >= 100
                then return PlayerState{turn = 1, player1 = p1, player2 = 100}
                else case Map.lookup (p2 + rollDice) obstacleDict of
                    Just exit -> return PlayerState{turn = 1, player1 = p1, player2 = (fromMaybe (p2 + rollDice) (Map.lookup (p2 + rollDice) obstacleDict))}
                    Nothing -> return PlayerState{turn = 1, player1 = p1, player2 = p2 + rollDice}
            else return PlayerState{turn = t, player1 = p1, player2 = p2}

--Updates the gamestate based on the player's state
updateGameState :: PlayerState -> GameState -> GameState
updateGameState (PlayerState {turn = t, player1 = p1, player2 = p2}) (GameState {grid = g, gameOver = o}) = 
    let p1x = div p1 10
        p1y = mod p1 10
        p2x = div p2 10
        p2y = mod p2 10
        grid = replicate gridSize $ replicate gridSize (False, False)
        updatedGrid = [[if (x, y) == (p1x, p1y) && (x, y) == (p2x, p2y) then (True, True)
                        else 
                            if (x, y) == (p2x, p2y) then (False, True)
                        else 
                            if (x, y) == (p1x, p1y) then (True, False)
                        else (False, False)
                        | y <- [0..gridSize-1]]
                        | x <- [0..gridSize-1]]
        gameOver1 = if p1 == 100 then 1 else o
        gameOver2 = if p2 == 100 then 2 else o
        gameOver' = max gameOver1 gameOver2
    in GameState{grid = updatedGrid, gameOver = gameOver'}
