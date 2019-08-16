{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes #-}

module Session2 where

import Prelude hiding (seq)

import Session1_Template

import Lens.Micro
import Lens.Micro.TH

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
  , _animations :: [Dsl (Ops World) ()]
  }

makeLenses ''World

upAnim :: Dsl (Ops World) ()
upAnim = basic (For 2) (sprites . atIndex 0 . y) (To 100)

downAnim :: Dsl (Ops World) ()
downAnim = basic (For 2) (sprites . atIndex 0 . y) (To 0)

fadeOut :: Dsl (Ops World) ()
fadeOut = let
  fadeScale = basic (For 2) (sprites . atIndex 0 . scale) (To 1.5)
  fadeAlpha = basic (For 2) (sprites . atIndex 0 . alpha) (To 0)
  in par [fadeScale, fadeAlpha]

{-
createParticle :: (Float, Float) -> World -> (World, Int)
createParticle (x, y) w@(World {_sprites, _nextSpriteId}) = let
  newIndex = w ^. nextSpr
  particle = Sprite x y 1 (1, 1, 1) 3 0 bombPic newIndex
  newWorld = w { _particles = particle : _particles, _nextParticleId = _nextParticleId + 1 }
  in (newWorld, newIndex)
-}

---------------------------
-- define gloss functions
---------------------------

drawSprite :: Sprite -> Picture
drawSprite Sprite {_x, _y, _alpha, _color, _scale, _rotation, _picture} = let
  (r, g, b) = _color
  in
  _picture
  & Color (makeColor r g b _alpha)
  & Scale _scale _scale
  & Rotate _rotation
  & Translate _x _y

draw :: World -> Picture
draw World{_sprites} = Pictures (map drawSprite _sprites)

handleInput :: Event -> World -> World
handleInput (EventKey (Char 'x') Down _ _) w =
  w & animations %~ \l -> fadeOut : l
handleInput (EventKey (Char 'c') Down _ _) w =
  w & animations %~ \l -> downAnim : l
handleInput _ w = w

update :: Float -> World -> World
update t w = let
  (newWorld, newAnimations) = applyOps w t (w ^. animations)
  cleanedAnims = cleanAnims newAnimations
  in newWorld & animations .~ cleanedAnims

sprite1 :: Sprite
sprite1 = let
  pic = ThickCircle 50 3
  in Sprite
    { _x = 10
    , _y = 10
    , _alpha = 1
    , _color = (0.8, 0.1, 0.2)
    , _scale = 1
    , _rotation = 0
    , _picture = pic
    }

initialWorld :: World
initialWorld = World
  { _sprites = [sprite1]
  , _animations = []
  }

main :: IO ()
main = let
  window = InWindow "animation-dsl" (400, 400) (50, 50)
  in play window black 60 initialWorld draw handleInput update


cleanAnims :: [Dsl (Ops obj) a] -> [Dsl (Ops obj) a]
cleanAnims l = let
  f (Return _) = False
  f _ = True
  in filter f l

atIndex :: Int -> Lens' [a] a
atIndex i = lens (!! i) (\s b -> take i s ++ b : drop (i+1) s)

