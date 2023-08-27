import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import System.Random

data GameState = GameState
  { barX :: Float,
    barY :: Float,
    isJumping :: Bool,
    isCrouching :: Bool,
    jumpTime :: Float,
    movingLeft :: Bool,
    movingRight :: Bool,
    obstacles :: [Obstacle],
    obstacleSpeed :: Float,
    score :: Int,
    maxScore :: Int,  -- Add the maxScore field
    playerImage1 :: Picture, -- Add this field for the player's image
    playerImage2 :: Picture,
    currentPlayerImage :: Picture,  -- Add this field to track the current player image
    imageAlternationTime :: Float
  }

data Obstacle = Obstacle
  { obstacleX :: Float,
    obstacleY :: Float,
    obstacleWidth :: Float,
    obstacleHeight :: Float
  }

data GameScreen = InitialScreen | RunningScreen | GameOverScreen

data GameData = GameData
  { gameState :: GameState,
    currentScreen :: GameScreen
  }

initialState :: GameData
initialState = GameData {gameState = initialGameState, currentScreen = InitialScreen}

initialGameState :: GameState
initialGameState = GameState {  barX = 0, 
                                barY = floorY + 25, 
                                isJumping = False, 
                                isCrouching = False, 
                                jumpTime = 0, 
                                movingLeft = False, 
                                movingRight = False, 
                                obstacles = [], 
                                obstacleSpeed = 150,
                                score = 0, 
                                maxScore = 0,
                                playerImage1 = Blank,
                                playerImage2 = Blank,
                                currentPlayerImage = Blank,
                                imageAlternationTime = 0
                                }

window :: Display
window = InWindow "Jumping Bar" (800, 600) (100, 100)

background :: Color
background = makeColorI 182 185 184 255  -- RGBA values for the color #B6B9B8

floorY :: Float
floorY = -250

defaultPlayerHeight :: Float
defaultPlayerHeight = 50

render :: GameData -> Picture
render gameData =
  case currentScreen gameData of
    InitialScreen ->
      pictures
        [ translate (-175) 0 $ scale 0.3 0.3 $ color black $ text "Press Enter to Start"
        ]
    RunningScreen ->
      pictures
        [ floorPic,
          obstaclesPic,
          playerPic,
          scorePic,
          maxScorePic  -- Add this line to display the max score
        ]
    GameOverScreen ->
      pictures
        [ translate (-175) 50 $ scale 0.3 0.3 $ color black $ text "Game Over",
          translate (-250) (-50) $ scale 0.3 0.3 $ color black $ text "Press Enter to Restart"
        ]
  where
    playerPic = translate (barX currentGameState) (barY currentGameState - defaultPlayerHeight/2 + playerHeight/2) $ currentPlayerImage currentGameState  -- Use currentPlayerImage here
    currentGameState = gameState gameData
    playerHeight  = if isCrouching currentGameState then defaultPlayerHeight * 0.5 else defaultPlayerHeight
    floorPic = translate 0 floorY $ color black $ rectangleSolid 800 20
    obstaclesPic = pictures $ map renderObstacle (obstacles currentGameState)
    scorePic = translate (-350) 250 $ scale 0.2 0.2 $ color black $ text $ "Score: " ++ show (score $ gameState gameData)
    maxScorePic = translate (-350) 220 $ scale 0.2 0.2 $ color black $ text $ "Max Score: " ++ show (maxScore $ gameState gameData)

renderObstacle :: Obstacle -> Picture
renderObstacle obstacle = translate (obstacleX obstacle) (obstacleY obstacle) $ color red $ rectangleSolid (obstacleWidth obstacle) (obstacleHeight obstacle)

handleInput :: Event -> GameData -> GameData
handleInput (EventKey (SpecialKey KeyRight) Down _ _) gameData@GameData{gameState = game} = gameData {gameState = game {movingRight = True}}
handleInput (EventKey (SpecialKey KeyRight) Up _ _) gameData@GameData{gameState = game} = gameData {gameState = game {movingRight = False}}
handleInput (EventKey (SpecialKey KeyLeft) Down _ _) gameData@GameData{gameState = game} = gameData {gameState = game {movingLeft = True}}
handleInput (EventKey (SpecialKey KeyLeft) Up _ _) gameData@GameData{gameState = game} = gameData {gameState = game {movingLeft = False}}
handleInput (EventKey (SpecialKey KeyUp) Down _ _) gameData@GameData{gameState = game}
  | not (isJumping game) = gameData {gameState = jump game}
  | otherwise = gameData
handleInput (EventKey (SpecialKey KeyDown) Down _ _) gameData@GameData{gameState = game} = gameData {gameState = game {isCrouching = True}}
handleInput (EventKey (SpecialKey KeyDown) Up _ _) gameData@GameData{gameState = game} = gameData {gameState = game {isCrouching = False}}
handleInput (EventKey (SpecialKey KeyEnter) Down _ _) gameData@GameData{currentScreen = InitialScreen} = gameData {currentScreen = RunningScreen}  -- Transition to RunningScreen
handleInput (EventKey (SpecialKey KeyEnter) Down _ _) gameData@GameData{currentScreen = GameOverScreen} =
  let gen = mkStdGen (maxScore (gameState gameData))
      newStateWithObstacles = generateInitialObstacles gen
      newMaxScore = max (score (gameState gameData)) (maxScore (gameState gameData))
      newStateWithImages = newStateWithObstacles { playerImage1 = playerImage1 (gameState gameData), playerImage2 = playerImage2 (gameState gameData) }
  in GameData {gameState = newStateWithImages {maxScore = newMaxScore}, currentScreen = RunningScreen}
handleInput _ gameData = gameData

update :: Float -> GameData -> GameData
update dt gameData@GameData{gameState = game}
  | checkCollision game = gameData {currentScreen = GameOverScreen}  -- Transition to GameOverScreen on collision
  | isJumping game && jumpTime game < 0.5 = gameData {gameState = updatedGame {barY = barY updatedGame + 200 * dt, jumpTime = jumpTime game + dt}}
  | isJumping game && jumpTime game >= 0.5 && barY updatedGame > floorY + 25 = gameData {gameState = updatedGame {barY = barY updatedGame - 200 * dt, jumpTime = jumpTime game + dt}}
  | isJumping game && barY updatedGame <= floorY + 25 = gameData {gameState = updatedGame {barY = floorY + 25, isJumping = False, jumpTime = 0}}
  | movingRight game = gameData {gameState = updatedGame {barX = barX game + 300 * dt}}
  | movingLeft game = gameData {gameState = updatedGame {barX = barX game - 300 * dt}}
  | otherwise = gameData {gameState = updatedGame {jumpTime = 0, maxScore = max (score updatedGame) (maxScore game)}}
  where
    updatedGame = updateObstacles dt (updateImageAlternationTime game dt)  -- Pass the updated time

updateImageAlternationTime :: GameState -> Float -> GameState
updateImageAlternationTime game dt =
  let newTime = imageAlternationTime game + dt
      (image1, image2) = (playerImage1 game, playerImage2 game)
      newPlayerImage = if even (floor newTime) then image1 else image2  -- Alternate images based on time
  in game { imageAlternationTime = newTime, currentPlayerImage = newPlayerImage }  -- Update currentPlayerImage

jump :: GameState -> GameState
jump game = game {isJumping = True, jumpTime = 0}

-- Define base obstacle space as a constant
baseObstacleSpace :: Float
baseObstacleSpace = 400

-- Modify the generateObstacles function
generateObstacles :: RandomGen g => g -> [Obstacle]
generateObstacles gen = obstacles
  where
    (numObstacles, gen') = randomR (5, 1000) gen
    obstacleWidth = 20
    obstacleXPositions = scanl (+) (800 + obstacleWidth) (take numObstacles (randomDistances gen'))
    obstacles = zipWith generateObstacle obstacleXPositions $ randoms gen'

randomDistances :: RandomGen g => g -> [Float]
randomDistances gen = distances
  where
    distances = map (\x -> baseObstacleSpace + x * 200) (randomRs (-1, 1) gen)

generateObstacle :: Float -> Float -> Obstacle
generateObstacle x rand = Obstacle
  { obstacleX = x,
    obstacleY = if rand < 0.5 then floorY + 25 else floorY + 20 + defaultPlayerHeight - (5 + rand * 10),
    obstacleWidth = 20,
    obstacleHeight = 5 + rand * 10
  }
  
speedIncreaseRate :: Float
speedIncreaseRate = 2

updateObstacles :: Float -> GameState -> GameState
updateObstacles dt game =
  let newSpeed = obstacleSpeed game + speedIncreaseRate * dt
      updatedGame = game {obstacles = map updateObstacle (obstacles game), obstacleSpeed = newSpeed}
  in updatedGame {obstacles = filter (\o -> obstacleX o > -400) (obstacles updatedGame), score = score game + numObstaclesPassed}
  where
    updateObstacle obstacle = obstacle {obstacleX = obstacleX obstacle - obstacleSpeed game * dt}
    numObstaclesPassed = length $ filter (\o -> obstacleX o + obstacleWidth o <= barX game) (obstacles game)

checkCollision :: GameState -> Bool
checkCollision game =
  any (\obstacle -> collidesWith (barX game) (barY game) (obstacleX obstacle) (obstacleY obstacle) (obstacleWidth obstacle) (obstacleHeight obstacle) game) (obstacles game)

collidesWith :: Float -> Float -> Float -> Float -> Float -> Float -> GameState -> Bool
collidesWith playerX playerY x y width height game =
  let playerHeight = if isCrouching game then defaultPlayerHeight * 0.5 else defaultPlayerHeight
      xOverlap = abs (playerX - x) * 2 <= (50 + width)
      yOverlap = abs (playerY - y) * 2 <= (playerHeight + height)
  in xOverlap && yOverlap

generateInitialObstacles :: RandomGen g => g -> GameState
generateInitialObstacles gen = initialGameState {obstacles = generateObstacles gen}

main :: IO ()
main = do
  gen <- newStdGen
  let initialStateWithObstacles = generateInitialObstacles gen
  playerImage1 <- loadBMP "images/walk_left.bmp"
  playerImage2 <- loadBMP "images/walk_right.bmp"
  let initialGameData = initialState { gameState = initialStateWithObstacles { playerImage1 = playerImage1, playerImage2 = playerImage2 }, currentScreen = InitialScreen }
  play window background 60 initialGameData render handleInput update
