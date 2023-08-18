import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import System.Random

data GameState = GameState
  { barX :: Float,
    barY :: Float,
    isJumping :: Bool,
    jumpTime :: Float,
    movingLeft :: Bool,
    movingRight :: Bool,
    obstacles :: [Obstacle]
  }

data Obstacle = Obstacle
  { obstacleX :: Float,
    obstacleY :: Float,
    obstacleWidth :: Float,
    obstacleHeight :: Float
  }

initialState :: GameState
initialState = GameState {barX = 0, barY = floorY + 25, isJumping = False, jumpTime = 0, movingLeft = False, movingRight = False, obstacles = []}

window :: Display
window = InWindow "Jumping Bar" (800, 600) (100, 100)

background :: Color
background = white

floorY :: Float
floorY = -250

render :: GameState -> Picture
render game = pictures [ barPic, floorPic, obstaclesPic ]
  where
    barPic = translate (barX game) (barY game) $ color black $ rectangleSolid 50 50
    floorPic = translate 0 floorY $ color black $ rectangleSolid 800 20
    obstaclesPic = pictures $ map renderObstacle (obstacles game)

renderObstacle :: Obstacle -> Picture
renderObstacle obstacle = translate (obstacleX obstacle) (obstacleY obstacle) $ color red $ rectangleSolid (obstacleWidth obstacle) (obstacleHeight obstacle)

handleInput :: Event -> GameState -> GameState
handleInput (EventKey (SpecialKey KeyRight) Down _ _) game = game {movingRight = True}
handleInput (EventKey (SpecialKey KeyRight) Up _ _) game = game {movingRight = False}
handleInput (EventKey (SpecialKey KeyLeft) Down _ _) game = game {movingLeft = True}
handleInput (EventKey (SpecialKey KeyLeft) Up _ _) game = game {movingLeft = False}
handleInput (EventKey (SpecialKey KeyUp) Down _ _) game
  | not (isJumping game) = jump game  -- Only allow jumping if not already jumping
  | otherwise = game
handleInput _ game = game

update :: Float -> GameState -> GameState
update dt game
  | checkCollision game = game -- Handle collision here (e.g., end the game)
  | isJumping game && jumpTime game < 0.5 = updatedGame {barY = barY updatedGame + 200 * dt, jumpTime = jumpTime game + dt}
  | isJumping game && jumpTime game >= 0.5 && barY updatedGame > floorY + 25 = updatedGame {barY = barY updatedGame - 200 * dt, jumpTime = jumpTime game + dt}
  | isJumping game && barY updatedGame <= floorY + 25 = updatedGame {barY = floorY + 25, isJumping = False, jumpTime = 0}
  | movingRight game = updatedGame {barX = barX game + 300 * dt}
  | movingLeft game = updatedGame {barX = barX game - 300 * dt}
  | otherwise = updatedGame {jumpTime = 0}
  where
    updatedGame = updateObstacles dt game

jump :: GameState -> GameState
jump game = game {isJumping = True, jumpTime = 0}

generateObstacles :: RandomGen g => g -> [Obstacle]
generateObstacles gen = obstacles
  where
    (numObstacles, gen') = randomR (5, 1000) gen
    obstacles = take numObstacles $ zipWith generateObstacle [1..] $ randoms gen'

generateObstacle :: Int -> Float -> Obstacle
generateObstacle index rand = Obstacle
  { obstacleX = 800 + fromIntegral index * 300,
    obstacleY = floorY + 25,
    obstacleWidth = 20,
    obstacleHeight = 5 + rand * 10
  }

updateObstacles :: Float -> GameState -> GameState
updateObstacles dt game = updatedGame { obstacles = map updateObstacle (obstacles game) }
  where
    updatedGame = game { obstacles = filter (\o -> obstacleX o > -400) (obstacles game) }
    updateObstacle obstacle = obstacle { obstacleX = obstacleX obstacle - 300 * dt }

checkCollision :: GameState -> Bool
checkCollision game =
  any (\obstacle -> collidesWith (barX game) (barY game) (obstacleX obstacle) (obstacleY obstacle) (obstacleWidth obstacle) (obstacleHeight obstacle)) (obstacles game)

collidesWith :: Float -> Float -> Float -> Float -> Float -> Float -> Bool
collidesWith x1 y1 x2 y2 width height =
  let xOverlap = abs (x1 - x2) * 2 <= (50 + width)
      yOverlap = abs (y1 - y2) * 2 <= (50 + height)
  in xOverlap && yOverlap


main :: IO ()
main = do
  gen <- newStdGen
  let initialStateWithObstacles = initialState {obstacles = generateObstacles gen}
  play window background 60 initialStateWithObstacles render handleInput update
