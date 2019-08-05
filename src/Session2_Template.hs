{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes #-}

module Session2_Template where

import Session1_Template

import Lens.Micro
import Lens.Micro.TH

import Graphics.Gloss hiding (scale, color)
import Graphics.Gloss.Interface.Pure.Game hiding (scale, color)

----------------------
-- define data types
----------------------

-- introduce extension:
-- * TemplateHaskell: can generate new code at compile time, automatically creates lenses for our data types

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

---------------------------
-- define gloss functions
---------------------------

-- introduce extension:
-- * NamedFieldPuns: can use Record {x} instead of Record {x = x}

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

handleInput1 :: Event -> World -> World
handleInput1 e w = w

update1 :: Float -> World -> World
update1 t w = w

sprite1 :: Sprite
sprite1 = let
  pic = Circle 50
  in Sprite
    { _x = 10
    , _y = 10
    , _alpha = 1
    , _color = (0.8, 0.1, 0.2)
    , _scale = 1
    , _rotation = 0
    , _picture = pic
    }

initialWorld1 :: World
initialWorld1 = World
  { _sprites = [sprite1]
  , _animations = []
  }

main1 :: IO ()
main1 = let
  window = InWindow "animation-dsl" (400, 400) (50, 50)
  in play window black 60 initialWorld1 draw handleInput1 update1

-----------
-- step 2
-----------

-- define helper functions

cleanAnims :: [Dsl (Ops obj) a] -> [Dsl (Ops obj) a]
cleanAnims l = let
  f (Return _) = False
  f _ = True
  in filter f l

atIndex :: Int -> Lens' [a] a
atIndex i = lens (!! i) (\s b -> take i s ++ b : drop (i+1) s)

-- next step

handleInput2 :: Event -> World -> World
handleInput2 e w = w

update2 :: Float -> World -> World
update2 t w = let
  (newWorld, newOps) = applyOps w t (w ^. animations)
  cleanedOps = cleanAnims newOps
  in newWorld & animations .~ cleanedOps

animation2 :: Dsl (Ops World) ()
animation2 = do
  basic (For 2) (sprites . atIndex 0 . scale) (To 2)
  basic (For 2) (sprites . atIndex 0 . scale) (To 1)

initialWorld2 :: World
initialWorld2 = World
  { _sprites = [sprite1]
  , _animations = [animation2]
  }

main2 :: IO ()
main2 = let
  window = InWindow "animation-dsl" (400, 400) (50, 50)
  in play window black 60 initialWorld2 draw handleInput2 update2

-----------
-- step 3
-----------

handleInput3 :: Event -> World -> World
handleInput3 e w = w

update3 :: Float -> World -> World
update3 t w = let
  (newWorld, newOps) = applyOps w t (w ^. animations)
  cleanedOps = cleanAnims newOps
  in newWorld & animations .~ cleanedOps

animation3 :: Dsl (Ops World) ()
animation3 = par
  [ basic (For 2) (sprites . atIndex 0 . x) (To (-40))
  , basic (For 2) (sprites . atIndex 0 . y) (To (-40))
  , basic (For 2) (sprites . atIndex 0 . scale) (To 0)
  , basic (For 2) (sprites . atIndex 0 . alpha) (To 0)
  , basic (For 2) (sprites . atIndex 1 . x) (To (60))
  , basic (For 2) (sprites . atIndex 1 . y) (To (-40))
  , basic (For 2) (sprites . atIndex 1 . scale) (To 0)
  , basic (For 2) (sprites . atIndex 1 . alpha) (To 0)
  , basic (For 2) (sprites . atIndex 2 . x) (To (-40))
  , basic (For 2) (sprites . atIndex 2 . y) (To (60))
  , basic (For 2) (sprites . atIndex 2 . scale) (To 0)
  , basic (For 2) (sprites . atIndex 2 . alpha) (To 0)
  , basic (For 2) (sprites . atIndex 3 . x) (To (60))
  , basic (For 2) (sprites . atIndex 3 . y) (To (60))
  , basic (For 2) (sprites . atIndex 3 . scale) (To 0)
  , basic (For 2) (sprites . atIndex 3 . alpha) (To 0)
  ]

initialWorld3 :: World
initialWorld3 = World
  { _sprites = [sprite1, sprite1, sprite1, sprite1]
  , _animations = [animation3]
  }

main3 :: IO ()
main3 = let
  window = InWindow "animation-dsl" (400, 400) (50, 50)
  in play window black 60 initialWorld3 draw handleInput3 update3

-----------
-- step 4
-----------

handleInput4 :: Event -> World -> World
handleInput4 (EventKey (Char 'x') Down _ _) w = w & animations %~ \x -> animation3 : x
handleInput4 e w = w

initialWorld4 :: World
initialWorld4 = World
  { _sprites = [sprite1, sprite1, sprite1, sprite1]
  , _animations = []
  }

main4 :: IO ()
main4 = let
  window = InWindow "animation-dsl" (400, 400) (50, 50)
  in play window black 60 initialWorld4 draw handleInput4 update3


-------------

main :: IO ()
main = main4
