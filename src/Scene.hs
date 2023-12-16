{-# LANGUAGE TemplateHaskell #-}

module Scene where

import Graphics
import Physics

import Paths_beacHball

import Control.Arrow
import Control.Lens
import Control.Monad
import Control.Monad.Reader
import Data.Maybe
import qualified SDL
import SDL.Font as TTF
import System.Random

sceneGravity = 2500 :: Float

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

data CountDown = CountDown {
  _countDownNumbers :: [GraphicsItem],
  _countDownItem :: Maybe GraphicsItem
}
makeLenses ''CountDown

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
  _menuItems :: [GraphicsItem],
  _countDown :: CountDown
}
makeLenses ''GameScene

instance Scene GameScene where
  traverseItems_ f s =
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
    forOf_ (countDown . countDownItem . _Just) s f *>
    pure ()
  clearColor _ = SDL.V4 155 220 255 255

data WindowContext = WindowContext {
  winWidth :: Float,
  winHeight :: Float,
  groundY :: Float,
  renderer :: SDL.Renderer
}

type ReaderIO env a = ReaderT env IO a

createPlayer :: SDL.Scancode -> SDL.Scancode -> SDL.Scancode ->
                (Float -> Float) -> ReaderIO WindowContext Player
createPlayer left right up xp = do
  (WindowContext width _ base r) <- ask
  item <- liftIO $ graphicsSpriteItem r =<< getDataFileName "potato_sml.png"
  let player = Player left right up object
  return $ set xPos (xp width) .
           set yPos base .
           set (playerObject . objGravity) sceneGravity .
           set (playerObject . objCollisionShape) (collisionCircle . view objItem) .
           set (playerObject . objItem . itemSprite . anchor) AnchorBottomMid .
           set (playerObject . objItem) item
           $ player

createPlayer1 :: ReaderIO WindowContext Player
createPlayer1 = createPlayer SDL.ScancodeA SDL.ScancodeD SDL.ScancodeS xp
  where xp = (/ 4)

createPlayer2 :: ReaderIO WindowContext Player
createPlayer2 = do
  let xp w = w * 3 / 4
  set (playerObject . objItem . itemSprite . spriteTransform)
      (Just $ SpriteTransform 0 (True, False))
    <$> createPlayer SDL.ScancodeLeft SDL.ScancodeRight SDL.ScancodeUp xp

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

createBall :: ReaderIO WindowContext Ball
createBall = do
  (WindowContext width height _ r) <- ask
  item <- liftIO $ graphicsSpriteItem r =<< getDataFileName "ball.png"
  rgen <- liftIO newStdGen
  let ball = Ball rgen 0 object
  return $ resetBall width height $
           set (ballObject . objGravity) (sceneGravity / 2) .
           set (ballObject . objItem . itemSprite . spriteTransform)
               (Just $ SpriteTransform 0 (False, False)) .
           set (ballObject . objCollisionShape) (collisionCircle . view objItem) .
           set (ballObject . objItem) item
           $ setRandomAV ball

createSun :: ReaderIO WindowContext GraphicsItem
createSun = do
  (WindowContext w _ _ r) <- ask
  set xPos (3 * w / 4) .
    set yPos 0 .
    set (itemSprite . anchor) AnchorTopMid
    <$> liftIO (graphicsSpriteItem r =<< getDataFileName "sun.png")

createBackground :: ReaderIO WindowContext GraphicsItem
createBackground = do
  (WindowContext w h _ r) <- ask
  (tex, twi, thi) <- liftIO $ loadTexture r =<< getDataFileName "background.png"
  let xp = w / 2
      yp = h
      tw = fromIntegral twi :: Float
      th = fromIntegral thi :: Float
      (sw, sh) = (round w, round (th * w / tw))
      sprite = Sprite tex AnchorBottomMid sw sh Nothing
  return GraphicsItem {
    _itemX = xp,
    _itemY = yp,
    _itemRenderItem = Just (RenderSprite sprite),
    _itemVisible = True
  }

createCloud :: FilePath -> ReaderIO WindowContext Object
createCloud path = do
  (WindowContext w h _ r) <- ask
  item <- liftIO $ graphicsSpriteItem r path
  xp <- liftIO $ randomRIO (- w / 2, w)
  yp <- liftIO $ randomRIO (0, h / 4)
  v <- liftIO $ randomRIO (2, 40)
  return $ set (objItem . itemSprite . anchor) AnchorTopLeft .
           set xPos xp .
           set yPos yp .
           set xVel v .
           set objItem item
           $ object

createClouds :: ReaderIO WindowContext [Object]
createClouds = do
  paths <- liftIO $ mapM getDataFileName ["cloud" ++ show n ++ ".png" | n <- [1..5]]
  mapM createCloud paths

createBounds :: ReaderIO WindowContext (Object, Object, Object)
createBounds = do
  (WindowContext width _ base _) <- ask
  let ground = set yPos base .
               set objCollisionShape (const . Just $ CollisionLine ((0, base), (0, -1)))
               $ object
      leftWall = set objCollisionShape (const . Just $ CollisionLine ((0, 0), (1, 0))) object
      rightWall = set xPos width .
                  set objCollisionShape (const . Just $ CollisionLine ((width, 0), (-1, 0)))
                  $ object
  return (ground, leftWall, rightWall)

createPole :: ReaderIO WindowContext (Object, Object, Object)
createPole = do
  (WindowContext width height base renderer) <- ask
  let getPoleSprite = return . RenderSprite . set anchor AnchorBottomMid <=< sprite renderer <=< getDataFileName
  poleSprite <- liftIO $ Just <$> getPoleSprite "pole.png"
  poleBackSprite <- liftIO $ Just <$> getPoleSprite "pole_back.png"
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

black = SDL.V4 0 0 0 255

createMenu :: TTF.Font -> ReaderIO WindowContext [GraphicsItem]
createMenu font = do
  (WindowContext width height _ renderer) <- ask
  newBall <- liftIO $ graphicsTextItem renderer font black "N: Neuer Ball"
  return [set (itemSprite . anchor) AnchorTopRight . set xPos (width - 10) . set yPos 10 $ newBall]

createCountDown :: TTF.Font -> ReaderIO WindowContext CountDown
createCountDown font = do
  (WindowContext width height _ renderer) <- ask
  let createNumber i = set xPos (width / 2) .
                       set yPos (height / 4) .
                       set (itemSprite . anchor) AnchorCenter
                       <$> graphicsTextItem renderer font black (show i)
  one <- liftIO $ createNumber 1
  two <- liftIO $ createNumber 2
  three <- liftIO $ createNumber 3
  return $ CountDown [one, two, three] Nothing

fontFilePath = getDataFileName "jellee-typeface/Jellee-Roman.ttf"
fontForSize s = flip TTF.load s =<< fontFilePath

startScene :: SDL.Window -> SDL.Renderer -> IO GameScene
startScene window renderer = do
  TTF.initialize
  menuFont <- fontForSize 18
  countDownFont <- fontForSize 36
  windowConfig <- SDL.getWindowConfig window
  let (SDL.V2 wi hi) = SDL.windowInitialSize windowConfig
      width = fromIntegral wi
      height = fromIntegral hi
      base = height - height / 7
      windowContext = WindowContext width height base renderer
  scene <- flip runReaderT windowContext $ do
    (ground, leftWall, rightWall) <- createBounds
    (backPole, net, frontPole) <- createPole
    p1 <- createPlayer1
    p2 <- createPlayer2
    sun <- createSun
    bg <- createBackground
    clouds <- createClouds
    ball <- createBall
    menu <- createMenu menuFont
    countDown <- createCountDown countDownFont
    return $ GameScene width height base ground leftWall rightWall backPole net frontPole
                       sun clouds bg ball p1 p2 menu countDown
  TTF.quit
  return scene
