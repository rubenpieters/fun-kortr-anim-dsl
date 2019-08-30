{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes #-}

module Session3_Exercise where

import Prelude hiding (seq)

import Session1_Template

import Data.Maybe (catMaybes)
import Data.List (find, findIndex, nub)

import Lens.Micro hiding (set)
import Lens.Micro.TH

import Graphics.Gloss hiding (scale, color)
import Graphics.Gloss.Interface.Pure.Game hiding (scale, color)

import Debug.Trace

----------------------
-- define data types
----------------------

type RGB = (Float, Float, Float)

data Sprite
  = Sprite
  { _x :: Float
  , _y :: Float
  , _alpha :: Float
  , _color :: RGB
  , _scale :: Float
  , _rotation :: Float
  , _picture :: Picture
  , _spriteId :: Int
  , _animation :: [Animation (Operation Sprite) ()]
  }

makeLenses ''Sprite

data World
  = World
  { _bgSprites :: [Sprite]
  , _bgAnimations :: [Animation (Operation World) ()]
  , _arrowSprites :: [Sprite]
  , _arrowAnimations :: [Animation (Operation World) ()]
  , _particleSprites :: [Sprite]
  , _particleAnimations :: [Animation (Operation World) ()]
  , _nextSpriteId :: Int
  }

makeLenses ''World

data Direction
  = DLeft
  | DDown
  | DUp
  | DRight
  deriving (Eq, Show, Enum)

directionX :: Direction -> Float
directionX DLeft = -90
directionX DDown = -30
directionX DUp = 30
directionX DRight = 90

directionRot :: Direction -> Float
directionRot DLeft = -90
directionRot DDown = 180
directionRot DUp = 0
directionRot DRight = 90

rotDirection :: Float -> Direction
rotDirection (-90) = DLeft
rotDirection 180 = DDown
rotDirection 0 = DUp
rotDirection 90 = DRight

directionIndex :: Direction -> Int
directionIndex DLeft = 0
directionIndex DDown = 1
directionIndex DUp = 2
directionIndex DRight = 3

---------------------------
-- define gloss functions
---------------------------

drawSprite :: Sprite -> Picture
drawSprite Sprite{ _x, _y, _alpha, _color, _scale, _rotation, _picture } = let
  (r, g, b) = _color
  in
  _picture &
  Color (makeColor r g b _alpha) &
  Scale _scale _scale &
  Rotate _rotation &
  Translate _x _y

draw :: World -> Picture
draw World{ _bgSprites, _arrowSprites, _particleSprites } =
  Pictures $
    (_bgSprites & map drawSprite) ++
    (_arrowSprites & map drawSprite) ++
    (_particleSprites & map drawSprite)

handleInput :: Event -> World -> World
handleInput (EventKey (Char 'd') Down _ _) w = handle DLeft w
handleInput (EventKey (Char 'f') Down _ _) w = handle DDown w
handleInput (EventKey (Char 'j') Down _ _) w = handle DUp w
handleInput (EventKey (Char 'k') Down _ _) w = handle DRight w
handleInput (EventKey (Char 'x') Down _ _) w =
  w & arrowAnimations %~ \l -> createMap m1 : l
handleInput (EventKey (SpecialKey KeySpace) Down _ _) w =
  w & particleAnimations %~ \l -> spaceParticle : l
handleInput e w = w

handle :: Direction -> World -> World
handle dir w = let
  ids = checkPress dir w
  in if null ids
       then w & bgSprites . atIndex (directionIndex dir) . animation .~ [wrongAnim dir]
       else w & particleAnimations %~ \l -> fadeArrows ids : l

checkPress :: Direction -> World -> [Sprite]
checkPress dir World{_arrowSprites} = let
  rot = directionRot dir
  condition arrow =
    arrow ^. rotation == rot &&
    arrow ^. y < (-120) &&
    arrow ^. y > (-180)
  arrows = filter condition _arrowSprites
  in arrows

update :: Float -> World -> World
update t w = let
  newWorld0 = w & arrowSprites . traverse %~ updateSprite t
  newWorld1 = newWorld0 & bgSprites . traverse %~ updateSprite t
  (newWorld2, newArrowOps) = execOps newWorld1 t (newWorld1 ^. arrowAnimations)
  (newWorld3, newParticleOps) = execOps newWorld2 t (newWorld2 ^. particleAnimations)
  (newWorld4, newBgOps) = execOps newWorld3 t (newWorld3 ^. bgAnimations)
  cleanedArrowOps = cleanAnims newArrowOps
  cleanedParticleOps = cleanAnims newParticleOps
  cleanedBgOps = cleanAnims newBgOps
  missedIds = [DLeft ..] >>= (\dir -> checkMiss dir newWorld4)
  in newWorld4
       & arrowAnimations .~ cleanedArrowOps
       & particleAnimations .~ columnAnim missedIds : cleanedParticleOps
       & bgAnimations .~ cleanedBgOps

checkMiss :: Direction -> World -> [Sprite]
checkMiss dir World{_arrowSprites} = let
  rot = directionRot dir
  condition arrow =
    arrow ^. rotation == rot &&
    arrow ^. y < (-190)
  arrows = filter condition _arrowSprites
  in arrows

updateSprite :: Float -> Sprite -> Sprite
updateSprite t sprite = let
  (newSprite, newAnim) = execOps sprite t (sprite ^. animation)
  cleanedAnims = cleanAnims newAnim
  in newSprite & animation .~ cleanedAnims

-- main

main :: IO ()
main = let
  window = InWindow "animation-dsl" (400, 400) (100, 100)
  in play window black 60 initialWorld draw handleInput update

-----------
-- config
-----------

-- initial world

initialBgSprites :: [Sprite]
initialBgSprites =
  [ bgArrowSprite DLeft (-150) (1, 1, 1) 1 20 (-1)
  , bgArrowSprite DDown (-150) (1, 1, 1) 1 20 (-2)
  , bgArrowSprite DUp (-150) (1, 1, 1) 1 20 (-3)
  , bgArrowSprite DRight (-150) (1, 1, 1) 1 20 (-4)
  , Sprite 0 0 1 (0, 0, 1) 1 0 (dividerPic DLeft) (-5) []
  , Sprite 0 0 1 (0, 0, 1) 1 0 (dividerPic DDown) (-6) []
  , Sprite 0 0 1 (0, 0, 1) 1 0 (dividerPic DUp) (-7) []
  , Sprite 0 0 1 (0, 0, 1) 1 0 (dividerPic DRight) (-8) []
  , Sprite 0 0 0 (1, 0, 0) 1 0 (columnPic DLeft) (-9) []
  , Sprite 0 0 0 (1, 0, 0) 1 0 (columnPic DDown) (-10) []
  , Sprite 0 0 0 (1, 0, 0) 1 0 (columnPic DUp) (-11) []
  , Sprite 0 0 0 (1, 0, 0) 1 0 (columnPic DRight) (-12) []
  ]

initialWorld :: World
initialWorld = World
  { _bgSprites = initialBgSprites
  , _bgAnimations = []
  , _arrowSprites = []
  , _arrowAnimations = []
  , _particleSprites = []
  , _particleAnimations = []
  , _nextSpriteId = 1
  }

-- pictures

trianglePic :: Picture
trianglePic = Polygon [(0, 1), (1, -1), (-1, -1)]

triangleWire :: Picture
triangleWire = lineLoop [(0, 1), (1, -1), (-1, -1)]

columnPic :: Direction -> Picture
columnPic dir =
  Polygon [(-30, 300), (30, 300), (30, -300), (-30, -300)] &
  Translate (directionX dir) 0

dividerPic :: Direction -> Picture
dividerPic dir =
  lineLoop [(-30, 300), (30, 300), (30, -300), (-30, -300)] &
  Translate (directionX dir) 0

arrowSprite :: Direction -> Float -> RGB -> Float -> Float -> Int -> Sprite
arrowSprite dir y color alpha scale i =
  Sprite (directionX dir) y alpha color scale (directionRot dir) trianglePic i []

bgArrowSprite :: Direction -> Float -> RGB -> Float -> Float -> Int -> Sprite
bgArrowSprite dir y color alpha scale i =
  Sprite (directionX dir) y alpha color scale (directionRot dir) triangleWire i []

-- particle helpers

createArrow :: Direction -> Float -> World -> (World, Int)
createArrow dir y w@(World {_arrowSprites, _nextSpriteId}) = let
  newIndex = _nextSpriteId
  arrow = Sprite (directionX dir) y 1 (1, 1, 1) 20 (directionRot dir) trianglePic newIndex [arrowTrajectory]
  newWorld = w { _arrowSprites = arrow : _arrowSprites, _nextSpriteId = _nextSpriteId + 1 }
  in (newWorld, newIndex)

deleteArrow :: Int -> World -> World
deleteArrow id w@(World {_arrowSprites}) = let
  newWorld = w { _arrowSprites = filter (\x -> x ^. spriteId /= id) _arrowSprites }
  in newWorld

-- create/delete example
createSpaceParticle :: (Float, Float) -> World -> (World, Int)
createSpaceParticle (x, y) w@(World {_particleSprites, _nextSpriteId}) = let
  newIndex = _nextSpriteId
  particle = Sprite x y 1 (1, 1, 1) 1 0 (circleSolid 40) newIndex []
  newWorld = w { _particleSprites = particle : _particleSprites, _nextSpriteId = _nextSpriteId + 1 }
  in (newWorld, newIndex)

createParticle :: (Float, Float) -> Float -> World -> (World, Int)
createParticle (x, y) rot w@(World {_particleSprites, _nextSpriteId}) = let
  newIndex = _nextSpriteId
  particle = Sprite x y 1 (0, 1, 0) 20 rot trianglePic newIndex []
  newWorld = w { _particleSprites = particle : _particleSprites, _nextSpriteId = _nextSpriteId + 1 }
  in (newWorld, newIndex)

deleteParticle :: Int -> World -> World
deleteParticle id w@World{_particleSprites} = let
  newWorld = w { _particleSprites = filter (\x -> x ^. spriteId /= id) _particleSprites }
  in newWorld

-- animation definitions

spaceParticle :: Animation (Operation World) ()
spaceParticle = error "undefined"

arrowAnim :: Direction -> Animation (Operation World) ()
arrowAnim dir = do
  i <- create (createArrow dir 175)
  delay (For 3.52)
  delete deleteArrow i

missAnim :: Animation (Operation Sprite) ()
missAnim =
  -- light up bg
  return ()

arrowTrajectory :: Animation (Operation Sprite) ()
arrowTrajectory = do
  -- lower scale
  basic (For 3) (y) (To (-220))

wrongAnim :: Direction -> Animation (Operation Sprite) ()
wrongAnim dir =
  -- move arrow back and forth
  return ()

fadeArrows :: [Sprite] -> Animation (Operation World) ()
fadeArrows arrows = let
  fade arrow = do
    delete deleteArrow (arrow ^. spriteId)
    -- create arrow particle
  in par (map fade arrows)

columnAnim :: [Sprite] -> Animation (Operation World) ()
columnAnim sprites = let
  dirs = nub (sprites & map _rotation & map rotDirection)
  anim dir = set (bgSprites . atIndex (8 + directionIndex dir) . animation) [missAnim]
  in par (dirs & map anim)

-- helper functions

atIndex :: Int -> Lens' [a] a
atIndex i = lens (!! i) (\s b -> take i s ++ b : drop (i+1) s)

withSpriteId :: Int -> Lens' [Sprite] Sprite
withSpriteId i = let
  get l = case find (\x -> x ^. spriteId == i) l of
    Just s -> s
    Nothing -> error ("no particle with index " ++ show i)
  set l x = case findIndex (\x -> x ^. spriteId == i) l of
    Just ix -> take ix l ++ x : drop (ix+1) l
    Nothing -> error ("no particle with index " ++ show i)
  in lens get set

-- map helper functions

createMap :: [String] -> Animation (Operation World) ()
createMap [] = delay (For 0)
createMap (str:r) = let
  parseChar dir c =
    if c == '0'
      then Nothing
      else Just (arrowAnim dir)
  parseStr str = catMaybes (map (\(c, dir) -> parseChar dir c) (zip str [DLeft ..]))
  in par [par (parseStr str), seq [delay (For 0.4), createMap r]]

m1 :: [String]
m1 =
  [ "0001"
  , "0010"
  , "0100"
  , "1000"
  , "0100"
  , "0010"
  , "0001"
  , "0010"
  , "0100"
  , "1000"
  , "0100"
  , "0010"
  , "0001"
  , "0010"
  , "0100"
  , "1000"
  , "0100"
  , "0010"
  , "0001"
  , "0010"
  , "0100"
  , "1000"
  , "0100"
  , "0010"
  , "0001"
  , "0010"
  , "0100"
  , "1000"
  , "0100"
  , "0010"
  , "0001"
  ]

m2 :: [String]
m2 =
  [ "0001"
  , "0000"
  , "0000"
  , "0000"
  , "0000"
  , "0000"
  , "0010"
  , "0000"
  , "0000"
  , "0000"
  , "0000"
  , "0000"
  , "0100"
  , "0000"
  , "0000"
  , "0000"
  , "0000"
  , "0000"
  , "1000"
  ]
