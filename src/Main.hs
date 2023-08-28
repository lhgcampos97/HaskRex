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
    maxScore :: Int, 
    playerImage1 :: Picture, 
    playerImage2 :: Picture,
    currentPlayerImage :: Picture,  
    imageAlternationTime :: Float,
    hasWon :: Bool
  }

data ObstacleType = FloorObstacle | FloatingObstacle deriving (Eq, Show)

data Obstacle = Obstacle
  { obstacleX :: Float,
    obstacleY :: Float,
    obstacleWidth :: Float,
    obstacleHeight :: Float,
    obstacleType :: ObstacleType, 
    obstacleImage :: Picture
  }

data GameScreen = InitialScreen | RunningScreen | GameOverScreen | WinnerScreen deriving (Eq)

data GameData = GameData
  { gameState :: GameState,
    currentScreen :: GameScreen,
    clouds :: [Cloud] 
  }

data Cloud = Cloud
  { cloudX :: Float,
    cloudY :: Float,
    cloudScale :: Float,
    cloudImage :: Picture  
  }

----- Initialize the game state with loaded images -----

initializeGameState :: GameState -> (Picture, Picture, Picture) -> GameState
initializeGameState gameState (playerImage1, playerImage2, obstacleImage) =
  gameState
    { playerImage1 = playerImage1,
      playerImage2 = playerImage2,
      obstacles = map (\obstacle -> obstacle {obstacleImage = obstacleImage}) (obstacles gameState)
    }

initialState :: GameData
initialState = GameData {gameState = initialGameState, currentScreen = InitialScreen, clouds = initialClouds cloudsImage }
  where
    cloudsImage = Blank


initialClouds :: Picture -> [Cloud]
initialClouds cloudImg =
  [ Cloud { cloudX = -300, cloudY = 200, cloudScale = 0.5, cloudImage = cloudImg }
  , Cloud { cloudX = -100, cloudY = 150, cloudScale = 0.4, cloudImage = cloudImg }
  , Cloud { cloudX = 100, cloudY = 180, cloudScale = 0.6, cloudImage = cloudImg }
  , Cloud { cloudX = 300, cloudY = 170, cloudScale = 0.3, cloudImage = cloudImg }
  , Cloud { cloudX = 500, cloudY = 190, cloudScale = 0.7, cloudImage = cloudImg }
  , Cloud { cloudX = -200, cloudY = 100, cloudScale = 0.4, cloudImage = cloudImg }
  , Cloud { cloudX = 0, cloudY = 130, cloudScale = 0.5, cloudImage = cloudImg }
  , Cloud { cloudX = 200, cloudY = 120, cloudScale = 0.6, cloudImage = cloudImg }
  , Cloud { cloudX = 400, cloudY = 140, cloudScale = 0.4, cloudImage = cloudImg }
  , Cloud { cloudX = 600, cloudY = 160, cloudScale = 0.5, cloudImage = cloudImg }
  ]

initialGameState :: GameState
initialGameState = GameState {  barX = 0, 
                                barY = floorY + 20, 
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
                                imageAlternationTime = 0,
                                hasWon = False
                                }

----- Set the value of the game structures and constants -----

window :: Display
window = InWindow "HaskRex" (800, 600) (100, 100)

background :: Color
background = white

floorY :: Float
floorY = -250

defaultPlayerHeight :: Float
defaultPlayerHeight = 25

baseObstacleSpace :: Float
baseObstacleSpace = 500

speedIncreaseRate :: Float
speedIncreaseRate = 2

----- Rendering ------

render :: GameData -> Picture
render gameData =
  case currentScreen gameData of
    InitialScreen ->
      pictures
        [ pictures (map renderCloud (clouds gameData)), 
          playerPic,
          translate (-135) 200 $ scale 0.5 0.5 $ color black $ text "HaskRex", 
          translate (-210) (-100) $ scale 0.3 0.3 $ color red $ text "Press Enter to Start",
          instructionText, 
          floorPic
        ]
    RunningScreen ->
      pictures
        [ playerPic,
          obstaclesPic,
          floorPic,
          scorePic,
          maxScorePic,
          pictures (map renderCloud (clouds gameData)) 
        ]
    GameOverScreen ->
      pictures
        [ playerPic,
          obstaclesPic,
          translate (-135) 50 $ scale 0.3 0.3 $ color black $ text "Game Over",
          translate (-230) (-50) $ scale 0.3 0.3 $ color red $ text "Press Enter to Restart"
        ]
    WinnerScreen ->
      pictures
        [ translate (-155) 90 $ scale 0.3 0.3 $ color black $ text "You're still here?",
          translate (-80) 40 $ scale 0.3 0.3 $ color black $ text "It's Over",
          translate (-155) (-10) $ scale 0.3 0.3 $ color black $ text "Go Home... Go!",
          playerPic,
          floorPic,
          pictures (map renderCloud (clouds gameData))
        ]
  where
    playerPic = translate (barX currentGameState) (barY currentGameState - defaultPlayerHeight/2 + playerHeight/2) $ currentPlayerImage currentGameState  -- Use currentPlayerImage here
    currentGameState = gameState gameData
    playerHeight  = if isCrouching currentGameState then defaultPlayerHeight * 0.5 else defaultPlayerHeight
    floorPic = translate 0 floorY $ color (makeColorI 83 83 83 255)  $ rectangleSolid 800 1
    obstaclesPic = pictures $ map renderObstacle (obstacles currentGameState)
    scorePic = translate (-350) 250 $ scale 0.2 0.2 $ color black $ text $ "Score: " ++ show (score $ gameState gameData)
    maxScorePic = translate (-350) 220 $ scale 0.2 0.2 $ color black $ text $ "Max Score: " ++ show (maxScore $ gameState gameData)
    instructionText = translate (-180) (-200) $ scale 0.1 0.1 $ color black $ text "Instructions: Press Up/Down Arrow to Jump/Crouch"

renderObstacle :: Obstacle -> Picture
renderObstacle obstacle = translate (obstacleX obstacle) (obstacleY obstacle) $ obstacleImage obstacle

renderCloud :: Cloud -> Picture
renderCloud cloud = translate (cloudX cloud) (cloudY cloud) $ scale (cloudScale cloud) (cloudScale cloud) (cloudImage cloud)


----- Handle the user's inputs -----

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
handleInput (EventKey (SpecialKey KeyEnter) Down _ _) gameData@GameData{currentScreen = GameOverScreen} = -- Transition to GameOverScreen 
  let gen = mkStdGen (maxScore (gameState gameData))
      newStateWithObstacles = generateInitialObstacles gen
      newMaxScore = max (score (gameState gameData)) (maxScore (gameState gameData))
      newStateWithImages = initializeGameState newStateWithObstacles images
  in GameData {gameState = newStateWithImages {maxScore = newMaxScore}, currentScreen = RunningScreen, clouds = clouds gameData} 
  where
    images = (playerImage1 (gameState gameData), playerImage2 (gameState gameData), obstacleImage (head (obstacles (gameState gameData))))
handleInput (EventKey (SpecialKey KeyEnter) Down _ _) gameData@GameData{currentScreen = WinnerScreen} = gameData  -- Transition to InitialScreen (Restart)  
handleInput _ gameData = gameData

----- Update the game state constantly and track the changes -----

update :: Float -> GameData -> GameData
update dt gameData@GameData{currentScreen = screen} =
  case screen of
    RunningScreen -> updateRunningScreen dt gameData
    _ -> gameData

updateRunningScreen :: Float -> GameData -> GameData
updateRunningScreen dt gameData@GameData{gameState = game}
  | score game >= 10000 = gameData {currentScreen = WinnerScreen, gameState = game {hasWon = True}}  -- Trigger the winner screen
  | checkCollision game = gameData {currentScreen = GameOverScreen}  -- Transition to GameOverScreen on collision
  | isJumping game && jumpTime game < 0.5 = gameData {gameState = updatedGame {barY = barY updatedGame + 200 * dt, jumpTime = jumpTime game + dt}} -- Simulate a jump rise and fall
  | isJumping game && jumpTime game >= 0.5 && barY updatedGame > floorY + 25 = gameData {gameState = updatedGame {barY = barY updatedGame - 200 * dt, jumpTime = jumpTime game + dt}}
  | isJumping game && barY updatedGame <= floorY + 25 = gameData {gameState = updatedGame {barY = floorY + 25, isJumping = False, jumpTime = 0}}
  | movingRight game = gameData {gameState = updatedGame {barX = barX game + 300 * dt}}
  | movingLeft game = gameData {gameState = updatedGame {barX = barX game - 300 * dt}}
  | otherwise = gameData {gameState = updatedGame {jumpTime = 0, maxScore = max (score updatedGame) (maxScore game)}}
  where
    updatedGame = updateObstacles dt (updateImageAlternationTime game dt)

updateImageAlternationTime :: GameState -> Float -> GameState
updateImageAlternationTime game dt =
  let newTime = imageAlternationTime game + dt
      (image1, image2) = (playerImage1 game, playerImage2 game)
      newPlayerImage = if even (floor newTime :: Int) then image1 else image2
  in game { imageAlternationTime = newTime, currentPlayerImage = newPlayerImage }

updateObstacles :: Float -> GameState -> GameState
updateObstacles dt game =
  let newSpeed = obstacleSpeed game + speedIncreaseRate * dt
      updatedGame = game {obstacles = map updateObstacle (obstacles game), obstacleSpeed = newSpeed}
  in updatedGame {obstacles = filter (\o -> obstacleX o > -400) (obstacles updatedGame), score = score game + numObstaclesPassed}
  where
    updateObstacle obstacle = obstacle {obstacleX = obstacleX obstacle - obstacleSpeed game * dt}
    numObstaclesPassed = length $ filter (\o -> obstacleX o + obstacleWidth o <= barX game) (obstacles game)

jump :: GameState -> GameState
jump game = game {isJumping = True, jumpTime = 0}

----- Obstacles generation -----

generateObstacles :: RandomGen g => g -> [Obstacle]
generateObstacles gen = map (\(x, r) -> generateObstacle x r (randomObstacleType r) obstacleImage) $ zip obstacleXPositions (randoms gen')
  where
    (numObstacles, gen') = randomR (700, 1000) gen
    obstacleWidth = 20
    obstacleXPositions = scanl (+) (800 + obstacleWidth) (take numObstacles (randomDistances gen'))
    obstacleImage = Blank
    randomObstacleType r
        | r < (- 0.5) = FloorObstacle
        | r > 0.5 = FloatingObstacle
        | otherwise = if even (floor (r * 10)) then FloorObstacle else FloatingObstacle

generateObstacle :: Float -> Float -> ObstacleType -> Picture -> Obstacle
generateObstacle x rand obstacleType image = Obstacle
  { obstacleX = x,
    obstacleY = if obstacleType == FloorObstacle then floorY + 13 else floorY + 40 + defaultPlayerHeight - (rand * 10),
    obstacleWidth = 15,
    obstacleHeight = 40,
    obstacleType = obstacleType, 
    obstacleImage = image
  }

generateInitialObstacles :: RandomGen g => g -> GameState
generateInitialObstacles gen = initialGameState {obstacles = generateObstacles gen}

randomDistances :: RandomGen g => g -> [Float]
randomDistances gen = distances
  where
    distances = map (\x -> baseObstacleSpace + x * 200) (randomRs (-1, 1) gen)

----- Collision checks -----

checkCollision :: GameState -> Bool
checkCollision game =
  any (\obstacle -> collidesWith (barX game) (barY game) (obstacleX obstacle) (obstacleY obstacle) (obstacleWidth obstacle) (obstacleHeight obstacle) game) (obstacles game)

collidesWith :: Float -> Float -> Float -> Float -> Float -> Float -> GameState -> Bool
collidesWith playerX playerY x y width height game =
  let playerHeight = if isCrouching game then defaultPlayerHeight * 0.5 else defaultPlayerHeight
      xOverlap = abs (playerX - x) * 2 <= (50 + width)
      yOverlap = abs (playerY - y) * 2 <= (playerHeight + height)
  in xOverlap && yOverlap


main :: IO ()
main = do
  gen <- newStdGen
  let initialStateWithObstacles = generateInitialObstacles gen
  playerImage1' <- loadBMP "./images/walk_left_white.bmp"
  playerImage2' <- loadBMP "./images/walk_right_white.bmp"
  floorObstacleImage' <- loadBMP "./images/obstacle1.bmp"
  floatingObstacleImage' <- loadBMP "./images/obstacle2.bmp"
  cloudsImage' <- loadBMP "./images/cloud.bmp" 
  let initialGameData = initialState { gameState = initialStateWithObstacles { playerImage1 = playerImage1', playerImage2 = playerImage2' }, currentScreen = InitialScreen, clouds = initialClouds cloudsImage' }
  let obstaclesWithImages = map (\obstacle -> if obstacleType obstacle == FloorObstacle then obstacle { obstacleImage = floorObstacleImage' } else obstacle { obstacleImage = floatingObstacleImage' }) (obstacles (gameState initialGameData))
  play window background 60 initialGameData { gameState = (gameState initialGameData) { obstacles = obstaclesWithImages } } render handleInput update