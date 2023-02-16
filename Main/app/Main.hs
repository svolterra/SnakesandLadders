module Main(main) where

import Graphics.Gloss

window :: Display
window = InWindow "Snakes and Ladders" (1200, 600) (50, 50)

background :: Color
background = white

drawing :: Picture
drawing = pictures [
    line [(-600, -600), (-600, 600)],
    line [(-500, -600), (-500, 600)],
    line [(-400, -600), (-400, 600)],
    line [(-300, -600), (-300, 600)],
    line [(-200, -600), (-200, 600)],
    line [(-100, -600), (-100, 600)],
    line [(0,-600), (0, 600)],
    line [(100, -600), (100, 600)],
    line [(200, -600), (200, 600)],
    line [(300, -600), (300, 600)],
    line [(400, -600), (400, 600)],
    line [(500, -600), (500, 600)],
    line [(600, -600), (600, 600)],

    line [(-1200, 300), (1200, 300)],
    line [(-1200, 200), (1200, 200)],
    line [(-1200, 100), (1200, 100)],
    line [(-1200, 0), (1200, 0)],
    line [(-1200, -100), (1200, -100)],
    line [(-1200, -200), (1200, -200)],
    line [(-1200, -300), (1200, -300)]
    ]

main :: IO ()
main = display window background drawing