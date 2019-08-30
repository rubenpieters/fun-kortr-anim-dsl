{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes #-}

module Session2_Exercise where

import Prelude hiding (seq)

import Session1_Template

import Lens.Micro hiding (set)
import Lens.Micro.TH hiding (set)

import Graphics.Gloss hiding (scale, color)
import Graphics.Gloss.Interface.Pure.Game hiding (scale, color)

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
  }

makeLenses ''Sprite

data World
  = World
  { _sprites :: [Sprite]
  , _animations :: [Animation (Operation World) ()]
  }

makeLenses ''World

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
draw World{ _sprites } = Pictures (_sprites & map drawSprite)

handleInput :: Event -> World -> World
handleInput (EventKey (Char 'x') Down _ _) w =
  w & animations %~ \l -> fadeOut : l
handleInput (EventKey (Char 'c') Down _ _) w =
  w & animations %~ \l -> (seq [upAnim, downAnim]) : l
handleInput (EventKey (Char 'v') Down _ _) w =
  w & animations %~ \l -> beat : l
handleInput (EventKey (Char 'r') Down _ _) w =
  w & animations %~ \l -> reset : l
handleInput _ w = w

update :: Float -> World -> World
update t w = let
  (newWorld, newAnimations) = execOps w t (w ^. animations)
  cleanedAnims = cleanAnims newAnimations
  in newWorld & animations .~ cleanedAnims

-- main

main :: IO ()
main = let
  window = InWindow "animation-dsl" (400, 400) (50, 50)
  in play window black 60 initialWorld draw handleInput update

-----------
-- config
-----------

circleSprite :: Sprite
circleSprite = let
  pic = ThickCircle 50 3
  in Sprite
    { _x = 0
    , _y = 0
    , _alpha = 1
    , _color = (0.8, 0.1, 0.2)
    , _scale = 1
    , _rotation = 0
    , _picture = pic
    }

barSprite :: Sprite
barSprite = let
  pic = Polygon [(-25, -25), (-25, 25), (25, 25), (25, -25)]
  in Sprite
    { _x = 0
    , _y = 0
    , _alpha = 1
    , _color = (0.8, 0.1, 0.2)
    , _scale = 1
    , _rotation = 0
    , _picture = pic
    }

initialWorld :: World
initialWorld = World
  { _sprites = [circleSprite]
  , _animations = []
  }

-- animation definitions

upAnim :: Animation (Operation World) ()
upAnim = error "not implemented"

downAnim :: Animation (Operation World) ()
downAnim = error "not implemented"

fadeOut :: Animation (Operation World) ()
fadeOut = error "not implemented"

beat :: Animation (Operation World) ()
beat = error "not implemented"

reset :: Animation (Operation World) ()
reset = error "not implemented"

-- helper functions

atIndex :: Int -> Lens' [a] a
atIndex i = lens (!! i) (\s b -> take i s ++ b : drop (i+1) s)
