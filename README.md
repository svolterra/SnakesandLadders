## Snakes 🐍 and Ladders 🪜

A Haskell implementation of the [snakes and ladders board game](https://en.wikipedia.org/wiki/Snakes_and_ladders) with an associated GUI.

<img alt="gui picture" height="450" src="https://user-images.githubusercontent.com/71746168/221506478-8b821078-365f-442b-a641-89b2b50fdc2c.png">

---

### How the Game Works:

This game can be played interactively where each person picks a player and rolls the dice on their turn! 

1. If it is your turn, press the "Press to Roll" button to start the game. The first square of the board will be red to indicate that both players are currently at the first position.
2. Pressing the "Press to Roll" button will generate a number of steps to be taken by the current player (in the range of 1 to 6 such as  with normal dice).
3. If the steps lead the player to a cell with a green upper right side, this indicates they can advance to the cell with the same number! (For example, if the green number says 10, player advances to cell 10). These numbers represent ladders.
4. If the steps lead the player with an **red** upper right side, that indicates that they must _retreat_ to the cell number indicated. These numbers represent snakes.
5. Finally, if the player advances to a cell with an empty upper right side, no action is taken and the turn is passed on to the next player.
6. Should the players ever wish to restart the game, they can simply press the reset button and the game will be back to its starting state.

If a player wins, they are announced as the winner of the game!

<img alt="blue winner" width="300" src="https://user-images.githubusercontent.com/71746168/221504468-33357d4d-27f7-4aec-adca-5c82b41e81b4.png">

---

### References: 

1. [This Haskell implementation of Minesweeper](https://github.com/declanherbertson/minesweeper)
2. [This Haskell implementation of the ConnectFour game](https://github.com/vandyliu/connect4-haskell)
3. [This article by Andrew Gibiansky](https://andrew.gibiansky.com/blog/haskell/haskell-gloss/)

All sources of reference and inspiration found [here](https://docs.google.com/document/d/1JYfqBtzIrWGATTjaafh8RAD90N9amT-cZ6sVGBNGL7M/edit?usp=sharing).

---

### To Run this Game:

1. Install Haskell and cabal.
2. Clone and cd into this repository.
3. Run `cabal run` from your terminal!

