{-# LANGUAGE TemplateHaskell #-}

module Scene where

import Graphics
import Physics

import Paths_beacHball

import Control.Arrow
import Control.Lens
import Control.Monad
import Data.Maybe
import qualified SDL
import SDL.TTF as TTF
import SDL.TTF.FFI as TTF.FFI
import System.Random

sceneGravity = 2500

-- Circle with radius = width and located at top of sprite
circleForSprite :: Position -> Position -> Sprite -> CollisionShape
circleForSprite x y s = CollisionCircle ((sx + r, sy + r), r)
  where
    r = fromIntegral (view w s) / 2
    (sx, sy) = spriteTopLeft x y s
collisionCircle :: GraphicsItem -> Maybe CollisionShape
collisionCircle item = circleForSprite x y <$>
                       preview itemSprite item
  where
    (x, y) = (view itemX item, view itemY item)

data Player = Player {
  _leftKey :: SDL.Scancode,
  _rightKey :: SDL.Scancode,
  _upKey :: SDL.Scancode,
  _playerObject :: Object
}
makeLenses ''Player

instance Located Player where
  xPos = playerObject . xPos
  yPos = playerObject . yPos

instance Moving Player where
  xVel = playerObject . xVel
  yVel = playerObject . yVel
  gravity = gravity . view playerObject
  collisionShape = collisionShape . view playerObject

data Ball = Ball {
  _ballRandomGen :: StdGen,
  _ballAV :: Float,
  _ballObject :: Object
}
makeLenses ''Ball

instance Located Ball where
  xPos = ballObject . xPos
  yPos = ballObject . yPos

instance Moving Ball where
  xVel = ballObject . xVel
  yVel = ballObject . yVel
  gravity = gravity . view ballObject
  collisionShape = collisionShape . view ballObject

-- ball must have sprite and transformation
ballA :: Traversal' Ball Float
ballA = ballObject . objItem . itemSprite . spriteTransform . _Just . transformAngle

-- Traversal' Ball (Float, Float) = forall f. Applicative f => ((Float, Float) -> f (Float, Float)) -> s -> f s
ballAFrame :: Traversal' Ball (Float, Float)
ballAFrame g ball = maybe (pure ball) (fmap setAFrame . g) getAFrame
 where
   getAFrame = (,) <$> preview ballA ball <*> Just (view ballAV ball)
   setAFrame (a, av) = set ballA a . set ballAV av $ ball

ballRandomR :: Random a => (a, a) -> Ball -> (a, Ball)
ballRandomR range b =
  let (a, g) = randomR range (view ballRandomGen b)
  in  (a, set ballRandomGen g b)

data GameScene = GameScene {
  _width :: Float,
  _height :: Float,
  _baseY :: Float,
  _ground :: Object,
  _leftWall :: Object,
  _rightWall :: Object,
  _backPole :: Object,
  _net :: Object,
  _frontPole :: Object,
  _sun :: GraphicsItem,
  _clouds :: [Object],
  _background :: GraphicsItem,
  _ball :: Ball,
  _player1 :: Player,
  _player2 :: Player,
  _menuItems :: [GraphicsItem]
}
makeLenses ''GameScene

instance Scene GameScene where
  forItems_ s f =
    f (view sun s) *>
    forOf_ (clouds . traverse . objItem) s f *>
    f (view background s) *>
    f (view (backPole . objItem) s) *>
    f (view (net . objItem) s) *>
    f (view (ball . ballObject . objItem) s) *>
    f (view (player1 . playerObject . objItem) s) *>
    f (view (player2 . playerObject . objItem) s) *>
    f (view (frontPole . objItem) s) *>
    forOf_ (menuItems . traverse) s f *>
    pure ()
  clearColor _ = SDL.V4 155 220 255 255

createPlayer :: SDL.Renderer -> Float -> SDL.Scancode -> SDL.Scancode -> SDL.Scancode
                -> IO Player
createPlayer r base left right up = do
  item <- graphicsSpriteItem r =<< getDataFileName "potato_sml.png"
  let player = Player left right up object
  return $ set yPos base .
           set (playerObject . objGravity) sceneGravity .
           set (playerObject . objCollisionShape) (collisionCircle . view objItem) .
           set (playerObject . objItem . itemSprite . anchor) AnchorBottomMid .
           set (playerObject . objItem) item
           $ player

createPlayer1 :: SDL.Renderer -> Float -> Float -> IO Player
createPlayer1 r width base = set xPos (width / 4)
  <$> createPlayer r base SDL.ScancodeA SDL.ScancodeD SDL.ScancodeW

createPlayer2 :: SDL.Renderer -> Float -> Float -> IO Player
createPlayer2 r width base =
  set (playerObject . objItem . itemSprite . spriteTransform)
      (Just $ SpriteTransform 0 (True, False)) .
  set xPos (width * 3 / 4)
  <$> createPlayer r base SDL.ScancodeLeft SDL.ScancodeRight SDL.ScancodeUp

setRandomAV :: Ball -> Ball
setRandomAV b =
  let (av, b') = ballRandomR (50, 300) b
      av' = if view xVel b' < 0 then -av else av
  in  set ballAV av' b'

resetBall :: Float -> Float -> Ball -> Ball
resetBall width height b =
  let (xv, b') = ballRandomR (-800, 800) b
  in  set xPos (width / 2) .
      set yPos (height / 3) .
      set xVel xv .
      set yVel 0
      $ setRandomAV b

createBall :: SDL.Renderer -> Float -> Float -> IO Ball
createBall r width height = do
  item <- graphicsSpriteItem r =<< getDataFileName "ball.png"
  rgen <- newStdGen
  let ball = Ball rgen 0 object
  return $ resetBall width height $
           set (ballObject . objGravity) (sceneGravity / 2) .
           set (ballObject . objItem . itemSprite . spriteTransform)
               (Just $ SpriteTransform 0 (False, False)) .
           set (ballObject . objCollisionShape) (collisionCircle . view objItem) .
           set (ballObject . objItem) item
           $ setRandomAV ball

createSun :: SDL.Renderer -> Float -> IO GraphicsItem
createSun r w = do
  item <- graphicsSpriteItem r =<< getDataFileName "sun.png"
  return $ set xPos (3 * w / 4) .
           set yPos 0 .
           set (itemSprite . anchor) AnchorTopMid
           $ item

createBackground :: SDL.Renderer -> Float -> Float -> IO GraphicsItem
createBackground r w h = do
  (tex, twi, thi) <- loadTexture r =<< getDataFileName "background.png"
  let xp = w / 2
      yp = h
      tw = fromIntegral twi :: Float
      th = fromIntegral thi :: Float
      (sw, sh) = (round w, round (th * w / tw))
      sprite = Sprite tex AnchorBottomMid sw sh Nothing
  return $ GraphicsItem xp yp (Just $ RenderSprite sprite)

createCloud :: SDL.Renderer -> Float -> Float -> FilePath -> IO Object
createCloud r w h path = do
  item <- graphicsSpriteItem r path
  xp <- randomRIO (- w / 2, w)
  yp <- randomRIO (0, h / 4)
  v <- randomRIO (2, 40)
  return $ set (objItem . itemSprite . anchor) AnchorTopLeft .
           set xPos xp .
           set yPos yp .
           set xVel v .
           set objItem item
           $ object

createClouds :: SDL.Renderer -> Float -> Float -> IO [Object]
createClouds r w h = do
  paths <- mapM getDataFileName ["cloud" ++ show n ++ ".png" | n <- [1..5]]
  mapM (createCloud r w h) paths

createBounds :: Float -> Float -> (Object, Object, Object)
createBounds width base = (ground, leftWall, rightWall)
  where
    ground = set yPos base .
             set objCollisionShape (const . Just $ CollisionLine ((0, base), (0, -1)))
             $ object
    leftWall = set objCollisionShape (const . Just $ CollisionLine ((0, 0), (1, 0))) object
    rightWall = set xPos width .
                set objCollisionShape (const . Just $ CollisionLine ((width, 0), (-1, 0)))
                $ object


createPole :: SDL.Renderer -> Float -> Float -> Float -> IO (Object, Object, Object)
createPole renderer width height base = do
  let getPoleSprite = return . RenderSprite . set anchor AnchorBottomMid <=< sprite renderer <=< getDataFileName
  poleSprite <- Just <$> getPoleSprite "pole.png"
  poleBackSprite <- Just <$> getPoleSprite "pole_back.png"
  let poleDistance = height / 7
      (netX, netY, netHeight) = (width / 2, base, -265)
      backPole = set yPos (netY - poleDistance / 2) .
                 set xPos netX .
                 set (objItem . itemRenderItem) poleBackSprite
                 $ object
      net = set objCollisionShape (const . Just $ CollisionLineSegment ((netX, netY), (netX, netY + netHeight))) .
            set (objItem . itemRenderItem) (Just $ RenderLine (LineInfo (0, netHeight - poleDistance / 2 + 10) (SDL.V4 120 120 120 255))) .
            set xPos netX .
            set yPos netY
            $ object
      frontPole = set yPos (netY + poleDistance / 2) .
                 set xPos netX .
                 set (objItem . itemRenderItem) poleSprite
                 $ object
  return (backPole, net, frontPole)

createMenu :: SDL.Renderer -> TTF.FFI.TTFFont -> Float -> Float -> IO [GraphicsItem]
createMenu renderer font width height = do
  newBall <- graphicsTextItem renderer font (SDL.V4 0 0 0 255) "N: Neuer Ball"
  return [set (itemSprite . anchor) AnchorTopRight . set xPos (width - 10) . set yPos 10 $ newBall]

startScene :: SDL.Window -> SDL.Renderer -> IO GameScene
startScene window renderer =
  TTF.withInit $ do
    menuFont <- flip TTF.openFont 18 =<< getDataFileName "jellee-typeface/Jellee-Roman.ttf"
    windowConfig <- SDL.getWindowConfig window
    let (SDL.V2 wi hi) = SDL.windowInitialSize windowConfig
        width = fromIntegral wi
        height = fromIntegral hi
        base = height - height / 7
        (ground, leftWall, rightWall) = createBounds width base
    (backPole, net, frontPole) <- createPole renderer width height base
    p1 <- createPlayer1 renderer width base
    p2 <- createPlayer2 renderer width base
    sun <- createSun renderer width
    bg <- createBackground renderer width height
    clouds <- createClouds renderer width height
    ball <- createBall renderer width height
    menu <- createMenu renderer menuFont width height
    return $ GameScene width height base ground leftWall rightWall backPole net frontPole
                       sun clouds bg ball p1 p2 menu
