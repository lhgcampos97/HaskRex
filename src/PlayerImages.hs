-- PlayerImages.hs
module PlayerImages where

import Graphics.Gloss

-- Load player images as constants
playerImage1 :: Picture
playerImage1 = loadBMP "images/walk_left.bmp"

playerImage2 :: Picture
playerImage2 = loadBMP "images/walk_right.bmp"